{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Concelo.SyncTree
  ( Serializer(serialize, encrypt)
  , SyncTree()
  , empty
  , chunkHeight
  , chunkBody
  , chunkName
  , chunkMembers
  , update
  , root ) where

import Database.Concelo.Control (get, patternFailure, with, updateM,
                                 set, bsShow, maybeToAction, eitherToAction,
                                 run)
import Control.Applicative ((<|>))
import Control.Monad (foldM)

import qualified Control.Lens as L
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import qualified Database.Concelo.Trie as T
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

data SyncTree a = SyncTree { getTreeByReverseKeyMember :: T.Trie Key (Chunk a)
                           , getTreeByHeightVacancy :: T.Trie Key (Chunk a)
                           , getTreeRoot :: Chunk a }

treeByReverseKeyMember :: L.Lens' (SyncTree a) (T.Trie Key (Chunk a))
treeByReverseKeyMember =
  L.lens getTreeByReverseKeyMember
  (\t v -> t { getTreeByReverseKeyMember = v })

treeByHeightVacancy =
  L.lens getTreeByHeightVacancy (\t v -> t { getTreeByHeightVacancy = v })

treeRoot = L.lens getTreeRoot (\t v -> t { getTreeRoot = v })

leafKey = "l"

groupKey = "g"

emptyGroup = Group (Cr.hash []) 1 T.empty BS.empty

empty = SyncTree T.empty (T.index byHeightVacancy [emptyGroup]) emptyGroup

root = (const () <$>) . byHeightVacancy . getTreeRoot

chunkHeight = \case
  Leaf {} -> 1
  Group { getGroupHeight = h } -> h

chunkBody = \case
  Leaf { getLeafSerialized = s } -> s
  Group { getGroupBody = b } -> b

chunkName = getGroupName

chunkMembers = getGroupMembers

findObsolete chunk =
  let path = byKey chunk in
  (T.findValue path <$> get (stateTree . treeByReverseKeyMember))
    >>= \case
    Nothing -> patternFailure
    Just group -> do
      obsolete <- get stateObsolete
      if T.member path obsolete then
        return ()
        else do
        Co.update stateObsolete $ T.union (byHeightVacancy group)

        Co.update stateNew
          $ T.union $ T.index byHeightVacancy $ getGroupMembers group

        findObsolete group

findObsoleteGroups = do
  get stateObsolete >>= mapM_ findObsolete
  obsolete <- get stateObsolete
  Co.update stateNew (T.subtract obsolete)

getGroupVacancy group = Pr.leafSize - BS.length (getGroupBody group)

byKey chunk@(Leaf _ path) = Pa.super leafKey $ fmap (const chunk) path
byKey chunk@(Group name _ _ _) = Pa.super groupKey $ Pa.singleton name chunk

byReverseKeyMember chunk =
  T.index byKey $ getGroupMembers chunk

byHeightVacancy chunk =
  Pa.super (bsShow (getGroupHeight chunk))
  $ byVacancy chunk

byVacancy chunk =
  Pa.super (bsShow $ getGroupVacancy chunk)
  $ Pa.singleton (getGroupName chunk) chunk

byName chunk =
  Pa.singleton (getGroupName chunk) chunk

byLeafPath chunk = const chunk <$> getLeafPath chunk

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
    plaintext <- oneExactly <$> (serialize' $ T.index getLeafPath members)

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

    if height == 0 || twoOrMore new then do
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
      else
      maybeToAction (Co.Exception "no root group found") (T.firstValue new)
      >>= set (stateTree . treeRoot)

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

updateIndex index trie = do
  new <- get stateNew
  obsolete <- get stateObsolete

  return $ T.union (T.index index new)
    $ T.subtract (T.index index obsolete) trie

updateTree = do
  updateM (stateTree . treeByReverseKeyMember) (updateIndex byReverseKeyMember)
  updateM (stateTree . treeByHeightVacancy) (updateIndex byHeightVacancy)

toTrie = flip T.union T.empty

split path =
  snd . foldr visit (0 :: Int, T.empty) <$> serialize' (toTrie path) where
    visit string (count, trie) =
      (count + 1, T.union (const (Leaf string path') <$> path') trie) where
        path' = Pa.super (bsShow count) path

toLeaves leaves path = flip T.union leaves . T.super "0" <$> split path

update tree obsolete new = do
  serializer <- S.get

  (_, state) <-
    eitherToAction $ run
    (do foldM toLeaves T.empty (T.paths obsolete) >>= set stateObsolete
        foldM toLeaves T.empty (T.paths new) >>= set stateNew

        findObsoleteGroups
        addNewGroups (0 :: Int)
        updateTree)
    $ State tree serializer T.empty T.empty

  S.put (getStateSerializer state)

  return (getStateTree state,
          getStateObsolete state,
          getStateNew state)
