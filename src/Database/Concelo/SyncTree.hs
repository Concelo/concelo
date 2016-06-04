{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Concelo.SyncTree
  ( Serializer(serialize, makeId, encrypt)
  , SyncTree()
  , getTreeLeaves
  , Chunk()
  , chunkToMessage
  , chunkToGroupMessage
  , empty
  , update
  , defragment
  , visit
  , idSize
  , nullId
  , root ) where

import Database.Concelo.Control (get, patternFailure, with, set, bsShow,
                                 maybeToAction, eitherToAction, run, bsRead,
                                 exception)
import Control.Applicative ((<|>))
import Control.Monad (foldM, when, forM_)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, fromJust)

-- import Debug.Trace

import qualified Control.Lens as L
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import qualified Data.Word as W
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.VTrie as VT
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.Control as Co
import qualified Database.Concelo.Bytes as B

class Serializer a s where
  serialize :: T.Trie BS.ByteString a ->
               Co.Action (s a) BS.ByteString

  makeId :: Co.Action (s a) BS.ByteString

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

             | Leaf { getLeafFragment :: BS.ByteString
                    , getLeafPath :: Pa.Path Key a
                    , getLeafId :: BS.ByteString
                    , getLeafIndex :: W.Word64
                    , getLeafCount :: W.Word64 }

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
  Leaf {} -> 0
  Group { getGroupHeight = h } -> h

chunkBody = \case
  Leaf { getLeafFragment = f } -> f
  Group { getGroupBody = b } -> b

chunkName = getGroupName

chunkMembers = getGroupMembers

visitRejects rejects = mapM_ visit $ M.pairs $ invert rejects where

  invert = T.foldrPathsAndValues visitList M.empty where
    visitList (path, list) result = foldr visitName result list where
      visitName name = M.modify name (Just . T.union path . fromMaybe T.empty)

  visit (name, trie) =
    (T.findValue (Pa.singleton name ()) <$> get (stateTree . treeByName))
    >>= \case
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
  chunk@(Leaf { getLeafPath = path }) ->
    Pa.super leafKey $ fmap (const chunk) path

  chunk@(Group { getGroupName = name }) ->
    Pa.super groupKey $ Pa.singleton name chunk

byFindKey = \case
  chunk@(Leaf { getLeafPath = path }) ->
    Pa.super leafKey $ fmap (const chunk) (Pa.init path)

  chunk@(Group { getGroupName = name }) ->
    Pa.super groupKey $ Pa.singleton name chunk

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
-- empty new group to minimize vacancy
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

zeroOne = B.fromWord64 (0::W.Word64) `BS.append` B.fromWord64 (1::W.Word64)

makeGroup members = (case firstValue members of
                        Nothing -> return Nothing
                        Just first -> fromChunks first) where
  fromChunks = \case
    Leaf { getLeafFragment = fragment
         , getLeafId = id
         , getLeafIndex = index
         , getLeafCount = count } -> fromLeaves fragment id index count

    Group { getGroupHeight = height } -> fromGroups height

  fromLeaves fragment id index count = do

    plaintext <- (BS.append id . BS.append zeroOne <$>)
                 . oneExactly . split
                 <$> (serialize'
                      $ T.index (snd
                                 . fromJust
                                 . Pa.sub
                                 . Pa.init
                                 . getLeafPath)
                      members)

    let fragmented = if single members then
                       Just (id
                             `BS.append` B.fromWord64 index
                             `BS.append` B.fromWord64 count
                             `BS.append` fragment)
                     else
                       Nothing

    case plaintext <|> fragmented of
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

-- todo: both chunkToMessage and chunkToGroupMessage (along with the
-- Pr.leaf and Pr.group functions they invoke) are convoluted, leaky
-- abstractions that need to be redesigned

chunkToMessage private level treeStream forestStream chunk =
  case chunkHeight chunk of
    0 -> patternFailure

    1 -> Pr.leaf private level treeStream forestStream $ chunkBody chunk

    _ -> Pr.group private level (chunkHeight chunk) treeStream forestStream
         $ foldr (\member -> (chunkName member :)) []
         $ chunkMembers chunk

chunkToGroupMessage private level treeStream forestStream chunk =
  case chunkHeight chunk of
    0 -> patternFailure

    1 -> let hashes = foldr
                      (\member -> ((Pa.keys (getLeafPath member) !! 2) :)) []
                      $ chunkMembers chunk
             leafLevel = bsShow (bsRead level - 1 :: Int)
         in
          Pr.group' private level (chunkHeight chunk) treeStream forestStream
          hashes
          (foldr (\hash -> T.union (Pa.toPath [leafLevel, hash] ())) T.empty
           $ hashes)

    _ -> Pr.group private level (chunkHeight chunk) treeStream forestStream
         $ foldr (\member -> (chunkName member :)) []
         $ chunkMembers chunk

nullId = BS.pack $ take idSize $ repeat '\0'

update :: Integer ->
          (BS.ByteString ->
           Co.Action s BS.ByteString) ->
          (BS.ByteString ->
           Maybe (T.Trie Key a)) ->
          (T.Trie BS.ByteString Pr.Message,
           T.Trie BS.ByteString Pr.Message) ->
          SyncTree a ->
          Co.Action s (SyncTree a)
update revision decrypt deserialize (obsolete, new) tree =
  do
    -- traceM ("obsolete: " ++ show obsolete ++ "; new: " ++ show new)

    (obsoleteChunks, leafSubset, namedSubset, _) <-
      foldM remove ([], getTreeLeaves tree, getTreeByName tree, T.empty)
      obsolete

    (newChunks, leaves, named, _) <-
      foldM add ([], leafSubset, namedSubset, T.empty) new

    let reverseKeyMember =
          updateIndex byReverseKeyMember getTreeByReverseKeyMember
          obsoleteChunks newChunks

        heightVacancy =
          updateIndex byHeightVacancy getTreeByHeightVacancy
          obsoleteChunks newChunks

    return $ SyncTree named reverseKeyMember heightVacancy leaves
  where
    add (groups, leaves, named, fragments) message = do
      (newGroups, newLeaves, newNamed, fragments')
        <- convert named fragments message

      return (newGroups ++ groups,
              VT.union revision newLeaves leaves,
              T.union newNamed named,
              fragments')

    remove (groups, leaves, named, fragments) message = do
      (obsoleteGroups, obsoleteLeaves, obsoleteNamed, fragments')
        <- convert named fragments message

      return (obsoleteGroups ++ groups,
              VT.subtract revision obsoleteLeaves leaves,
              T.subtract obsoleteNamed named,
              fragments')

    toLeaves id count fragments path = T.foldrPaths visit T.empty fragments

      where path' index = Pa.super (bsShow index) path

            visit = \case
              Pa.Path [indexString] fragment ->
                let index = (B.toWord64 indexString) :: W.Word64 in
                T.union $ const (Leaf fragment (path' index) id index count)
                <$> (path' index)

              _ -> undefined

    distinguish distinguisher =
      T.foldrPaths (T.union . flip Pa.append distinguisher) T.empty

    convert named fragments = \case
      Pr.Group { Pr.getGroupName = name
               , Pr.getGroupMembers = members } -> do

        let visit result path =
              case T.findValue (Pa.singleton (last $ Pa.keys path) ()) named of
                Nothing -> exception ("can't find " ++ show path
                                      ++ " in " ++ show (const () <$> named))
                Just chunk -> return $ T.union (byName chunk) result

        memberChunks <- foldM visit T.empty $ T.paths members

        let hash = (Pa.keys name !! 2)
            group = Group hash (bsRead (Pa.keys name !! 1)) memberChunks
                    undefined

        return ([group], T.empty, T.singleton hash group, fragments)

      Pr.Leaf { Pr.getLeafName = name
              , Pr.getLeafBody = body } -> do

        ((maybeTrie, fragments'), (id, count, myFragments))
          <- defragment' deserialize fragments <$> decrypt body

        return $ case maybeTrie of
          Nothing ->
            ([], T.empty, T.empty, fragments')

          Just trie ->
            let hash = Pa.keys name !! 2
                leaves = distinguish hash trie
                members = T.foldrPaths
                          (T.union . toLeaves id count myFragments)
                          T.empty leaves
                group = Group hash (bsRead (Pa.keys name !! 1)) members
                        undefined
            in ([group],
                T.sub "0" leaves,
                T.singleton hash group,
                fragments')

      Pr.Tree { Pr.getTreeName = name } -> do
        let hash = Pa.keys name !! 1
            trie = fromMaybe T.empty $ deserialize hash

        return ([],
                T.sub "0" $ distinguish BS.empty trie,
                (\v -> Leaf undefined (const v <$> name) nullId 0 1) <$> trie,
                fragments)

      _ -> patternFailure

    updateIndex index accessor obsoleteChunks newChunks =
      (foldr (T.union . index)
       (foldr (T.subtract . index)
        (accessor tree)
        obsoleteChunks)
       newChunks)

defragment deserialize fragments fragment =
  fst $ defragment' deserialize fragments fragment

defragment' deserialize fragments fragment =
  ((if fromIntegral count == length myFragments then Just trie else Nothing,
    fragments'), (id, count, myFragments))
  where
    (id, tail) = BS.splitAt idSize fragment
    (indexString, tail') = BS.splitAt wordSize tail
    (countString, body) = BS.splitAt wordSize tail'
    count = B.toWord64 countString
    fragments' = T.union (Pa.super id $ Pa.singleton indexString body)
                 fragments
    myFragments = T.sub id fragments'

    -- todo: reject this leaf (i.e. add to rejected set) if the trie
    -- is empty:
    trie = fromMaybe T.empty $ deserialize (BS.concat $ toList myFragments)

toTrie = flip T.union T.empty

idSize = 32

wordSize = 8

headerSize = idSize + (wordSize * 2)

split s =
  let (a, b) = BS.splitAt (Pr.leafSize - headerSize) s in
  if BS.null b then
    [a]
  else
    a : split b

fragment path =
  do fragments <- split <$> serialize' (toTrie path)
     id <- with stateSerializer makeId

     return $ visit id (fromIntegral $ length fragments) (0::W.Word64)
       fragments
  where
    path' index = Pa.super (bsShow index) (Pa.append path BS.empty)

    visit id count = visit' where
      visit' index = \case
        [] -> []
        x:xs -> Leaf x (path' index) id index count : visit' (index + 1) xs

toLeaves leaves path =
  flip T.union leaves
  . T.super "0"
  . T.index (\v -> const v <$> getLeafPath v)
  <$> fragment path

visit :: (Show a, Serializer a s) =>
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

      ordered = T.union (T.index byHeightVacancy newGroups)
                $ T.subtract (T.index byHeightVacancy obsoleteGroups)
                (getTreeByHeightVacancy $ getStateTree state)

      root = T.lastValue ordered

  -- traceM ("root is " ++ show (T.lastPath ordered))
  -- traceM ("ordered is " ++ show ordered)

  return (obsoleteGroups, newGroups, root)
