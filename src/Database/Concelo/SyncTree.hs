{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
module Database.Concelo.SyncTree
  ( Serializer(serialize, encrypt)
  , SyncTree()
  , getTreeLeaves
  , Chunk()
  , chunkToMessage
  , empty
  , update
  , visit
  , root ) where

import Database.Concelo.Control (get, patternFailure, with, set, bsShow,
                                 maybeToAction, eitherToAction, run, bsRead)
import Control.Applicative ((<|>))
import Control.Monad (foldM, when, forM_)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

import qualified Control.Lens as L
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.VTrie as VT
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.Control as Co

class Serializer a s where
  serialize :: T.Trie BS.ByteString a ->
               Co.Action (s a) [BS.ByteString]

  encrypt :: BS.ByteString ->
             Co.Action (s a) BS.ByteString

type Key = BS.ByteString

data State a s =
  State { getStateTree :: SyncTree a
        , getStateSerializer :: s
        , getStateObsolete :: T.Trie Key (Chunk a)
        , getStateNew :: T.Trie Key (Chunk a) }

stateTree =
  L.lens getStateTree (\e v -> e { getStateTree = v })

stateSerializer :: L.Lens' (State a s) s
stateSerializer =
  L.lens getStateSerializer (\e v -> e { getStateSerializer = v })

stateObsolete :: L.Lens' (State a s) (T.Trie Key (Chunk a))
stateObsolete =
  L.lens getStateObsolete (\e v -> e { getStateObsolete = v })

stateNew :: L.Lens' (State a s) (T.Trie Key (Chunk a))
stateNew =
  L.lens getStateNew (\e v -> e { getStateNew = v })

data Chunk a = Group { getGroupName :: Key
                     , getGroupHeight :: Int
                     , getGroupMembers :: T.Trie Key (Chunk a)
                     , getGroupBody :: BS.ByteString }

             | Leaf { getLeafSerialized :: BS.ByteString
                    , getLeafPath :: Pa.Path Key a }

             deriving Show

data SyncTree a = SyncTree { getTreeByName :: T.Trie Key (Chunk a)
                           , getTreeByReverseKeyMember :: T.Trie Key (Chunk a)
                           , getTreeByHeightVacancy :: T.Trie Key (Chunk a)
                           , getTreeLeaves :: VT.VTrie Key a }

treeByName = L.lens getTreeByName (\t v -> t { getTreeByName = v })

treeByReverseKeyMember :: L.Lens' (SyncTree a) (T.Trie Key (Chunk a))
treeByReverseKeyMember =
  L.lens getTreeByReverseKeyMember
  (\t v -> t { getTreeByReverseKeyMember = v })

treeByHeightVacancy =
  L.lens getTreeByHeightVacancy (\t v -> t { getTreeByHeightVacancy = v })

leafKey = "l"

groupKey = "g"

empty = SyncTree T.empty T.empty T.empty VT.empty

root = fromMaybe (Pa.leaf ())
       . (fmap (const ()) <$>)
       . T.lastPath
       . getTreeByHeightVacancy

chunkHeight = \case
  Leaf {} -> 1
  Group { getGroupHeight = h } -> h

chunkBody = \case
  Leaf { getLeafSerialized = s } -> s
  Group { getGroupBody = b } -> b

chunkName = getGroupName

chunkMembers = getGroupMembers

visitRejects rejects = mapM_ visit $ M.pairs $ invert rejects where

  invert = T.foldrPathsAndValues visitList M.empty where
    visitList (path, list) result = foldr visitName result list where
      visitName name = M.modify name (Just . T.union path . fromMaybe T.empty)

  visit (name, trie) =
    (T.findValue (Pa.singleton name ())
     <$> get (stateTree . treeByName)) >>= \case
      Nothing -> return ()
      Just group -> do
        Co.update stateObsolete $ T.union $ byHeightVacancy group

        Co.update stateNew
          $ T.union $ T.index byHeightVacancy
          $ T.subtract trie $ getGroupMembers group

        findObsolete group

findObsolete chunk = do
  let path = byFindKey chunk

  trie <- T.findTrie path <$> get (stateTree . treeByReverseKeyMember)

  forM_ trie $ \group -> do
    let groupPath = byHeightVacancy group

    obsolete <- get stateObsolete

    if T.member groupPath obsolete then
      return ()
      else do
      Co.update stateObsolete $ T.union groupPath

      Co.update stateNew
        $ T.union $ T.index byHeightVacancy $ getGroupMembers group

      findObsolete group

findObsoleteGroups rejects = do
  get stateObsolete >>= mapM_ findObsolete

  visitRejects rejects

  obsolete <- get stateObsolete

  Co.update stateNew (T.subtract obsolete)

getGroupVacancy group = Pr.leafSize - BS.length (getGroupBody group)

byKey = \case
  chunk@(Leaf _ path) -> Pa.super leafKey $ fmap (const chunk) path
  chunk@(Group name _ _ _) -> Pa.super groupKey $ Pa.singleton name chunk

byFindKey = \case
  chunk@(Leaf _ path) -> Pa.super leafKey $ fmap (const chunk) (Pa.init path)
  chunk@(Group name _ _ _) -> Pa.super groupKey $ Pa.singleton name chunk

byReverseKeyMember group =
  const group <$> (T.index byKey $ getGroupMembers group)

byHeightVacancy group =
  Pa.super (bsShow $ getGroupHeight group)
  $ byVacancy group

byVacancy group =
  Pa.super (bsShow $ getGroupVacancy group)
  $ Pa.singleton (getGroupName group) group

byName group =
  Pa.singleton (getGroupName group) group

byLeafPath leaf = const leaf <$> Pa.init (getLeafPath leaf)

-- attempt to combine the most empty existing group with the most
-- empty new group to minimize fragmentation
combineVacant new old =
  case (,) <$> T.firstValue old <*> T.firstValue new of
    Nothing -> return (new, T.empty)

    Just (oldFirst, newFirst) ->
      (makeGroup $ T.union (getGroupMembers oldFirst)
        (getGroupMembers newFirst)) >>= \case
        Just combination ->
          return (T.union (byVacancy combination)
                  $ T.subtract (byVacancy newFirst) new,
                  toTrie $ byVacancy oldFirst)

        Nothing -> return (new, T.empty)

oneExactly = \case
  [x] -> Just x
  _ -> Nothing

firstValue = extract . foldr (:) [] where
  extract = \case
    x:_ -> Just x
    _ -> Nothing

single = check . foldr (:) [] where
  check = \case
    [_] -> True
    _ -> False

twoOrMore = check . foldr (:) [] where
  check = \case
    _:_:_ -> True
    _ -> False

serialize' trie = with stateSerializer $ serialize trie

encrypt' text = with stateSerializer $ encrypt text

makeGroup members = (case firstValue members of
                        Nothing -> return Nothing
                        Just first -> fromChunks first) where
  fromChunks = \case
    (Leaf serialized _) -> fromLeaves serialized
    (Group _ height _ _) -> fromGroups height

  fromLeaves first = do
    plaintext <- oneExactly <$>
                 (serialize' $ T.index (Pa.init . getLeafPath) members)

    let fragment = if single members then Just first else Nothing

    case plaintext <|> fragment of
      Nothing ->
        return Nothing

      Just text -> do
        ciphertext <- encrypt' text
        return $ Just $ Group (Cr.hash [ciphertext]) 1
          (T.index byLeafPath members) ciphertext

  fromGroups height = return $ do
    list <- collect 0 $ F.toList members

    Just $ Group (Cr.hash list) (height + 1) (T.index byName members)
      BS.empty where

      collect count = \case
        member:members ->
          let name = getGroupName member
              count' = count + BS.length name in

          if count' > Pr.leafSize then
            Nothing
          else
            (name:) <$> collect count' members

        _ -> Just []

isolate key = T.super key . T.sub key

addNewGroups height =
  do
    let heightKey = bsShow height

    new <- isolate heightKey <$> get stateNew

    when (height == 0 || twoOrMore new) $ do
      -- todo: consider (psuedo)randomly inserting new leaves among
      -- existing and new groups.  Is there a way to do this that
      -- doesn't result in disproportionate network traffic?

      newGroups <- foldM group T.empty $ T.paths new

      obsolete <- T.sub aboveKey <$> get stateObsolete

      oldGroups <- T.subtract obsolete . T.sub aboveKey
                   <$> get (stateTree . treeByHeightVacancy)

      (newGroups', obsoleteGroups') <- combineVacant newGroups oldGroups

      Co.update stateObsolete (T.union $ T.super aboveKey obsoleteGroups')
      Co.update stateNew (T.union $ T.super aboveKey newGroups')

      addNewGroups above

  where
    above = height + 1
    aboveKey = bsShow above

    group groups orphan = do
      orphanGroup <-
        makeGroup orphan >>=
        maybeToAction (Co.Exception "could not make a single-member group")

      case T.firstValue groups of
        Nothing ->
          return $ toTrie $ byHeightVacancy orphanGroup

        Just group ->
          (makeGroup $ T.union orphan $ getGroupMembers group) >>= \case
            Nothing -> return $ T.union (byHeightVacancy orphanGroup) groups

            Just combined ->
              return
              $ T.union (byHeightVacancy combined)
              $ T.subtract (byHeightVacancy group) groups

chunkToMessage private level treeStream forestStream chunk =
  case chunkHeight chunk of
    0 -> patternFailure

    1 -> Pr.leaf private level treeStream forestStream $ chunkBody chunk

    _ -> Pr.group private level (chunkHeight chunk) treeStream forestStream
         $ foldr (\member -> (chunkName member :)) []
         $ chunkMembers chunk

update revision deserialize (obsolete, new) tree =
  do
    rec (obsoleteChunks, leafSubset) <-
          foldM (remove $ getTreeByName tree) ([], getTreeLeaves tree) obsolete

        (newChunks, leaves) <- foldM (add named) ([], leafSubset) new

        named <-
          return $ updateIndex byName getTreeByName obsoleteChunks newChunks

    let reverseKeyMember =
          updateIndex byReverseKeyMember getTreeByReverseKeyMember
          obsoleteChunks newChunks

        heightVacancy =
          updateIndex byHeightVacancy getTreeByHeightVacancy
          obsoleteChunks newChunks

    return $ SyncTree named reverseKeyMember heightVacancy leaves
  where
    add nameTrie (groups, leaves) message = do
      (newGroups, newLeaves) <- convert nameTrie message

      return (newGroups ++ groups,
              VT.union revision newLeaves leaves)

    remove nameTrie (groups, leaves) message = do
      (obsoleteGroups, obsoleteLeaves) <- convert nameTrie message

      return (obsoleteGroups ++ groups,
              VT.subtract revision obsoleteLeaves leaves)

    convert nameTrie = \case
      Pr.Group { Pr.getGroupName = name
               , Pr.getGroupMembers = members } -> do

        let visit result path =
              case T.findValue (Pa.singleton (Pa.keys path !! 2) ()) nameTrie
              of
                Nothing -> patternFailure
                Just chunk -> return $ T.union (byName chunk) result

        memberChunks <- foldM visit T.empty $ T.paths members

        return ([Group (Pa.keys name !! 2) (bsRead (Pa.keys name !! 1))
                 memberChunks undefined],
                T.empty)

      Pr.Leaf { Pr.getLeafName = name
              , Pr.getLeafBody = body } -> do
        -- todo: handle fragmentation
        trie <- fromMaybe T.empty <$> deserialize body
        -- todo: reject this leaf if the trie is empty

        let hash = Pa.keys name !! 2
            leaves = T.foldrPaths (T.union . flip Pa.append hash) T.empty trie
            members = T.foldrPaths (T.union . toLeaf) T.empty leaves
            toLeaf path = const (Leaf undefined path) <$> path

        return ([Group hash (bsRead (Pa.keys name !! 1)) members undefined],
                leaves)

      _ -> return ([], T.empty)

    updateIndex index accessor obsoleteChunks newChunks =
      (foldr (T.union . index)
       (foldr (T.subtract . index)
        (accessor tree)
        obsoleteChunks)
       newChunks)

toTrie = flip T.union T.empty

split path =
  snd . foldr visit (0 :: Int, T.empty) <$> serialize' (toTrie path) where
    visit string (count, trie) =
      (count + 1, T.union (const (Leaf string path') <$> path') trie) where
        path' = Pa.super (bsShow count) (Pa.append path BS.empty)

toLeaves leaves path = flip T.union leaves . T.super "0" <$> split path

visit :: Serializer a s =>
         SyncTree a ->
         T.Trie BS.ByteString [BS.ByteString] ->
         T.Trie BS.ByteString a ->
         T.Trie BS.ByteString a ->
         Co.Action (s a) ([Chunk a],
                          [Chunk a],
                          Maybe (Chunk a))
visit tree rejects obsolete new = do
  serializer <- S.get

  (_, state) <-
    eitherToAction $ run
    (do foldM toLeaves T.empty (T.paths obsolete) >>= set stateObsolete
        foldM toLeaves T.empty (T.paths new) >>= set stateNew

        findObsoleteGroups rejects
        addNewGroups (0 :: Int))
    $ State tree serializer T.empty T.empty

  S.put (getStateSerializer state)

  let isGroup = \case
        Group {} -> True
        _ -> False

      groups = filter isGroup . toList

      newGroups = groups $ getStateNew state
      obsoleteGroups = groups $ getStateObsolete state

      root = (T.lastValue
              $ T.union (T.index byName newGroups)
              $ T.subtract (T.index byName obsoleteGroups)
              (getTreeByName $ getStateTree state))

  return (obsoleteGroups, newGroups, root)
