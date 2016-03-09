module Database.Concelo.SyncTree
  ( Serializer(serialize, encrypt)
  , empty ) where

import Database.Concelo.Control (Action, get, update, patternFailure,
                                 exception)

import qualified Control.Lens as L
import qualified Control.Monad.State.Class as S
import qualified Data.ByteString as BS
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Path as P
import qualified Database.Concelo.Crypto as C

class Serializer a where
  serialize :: Trie b -> Action a [BS.ByteString]
  encrypt :: BS.ByteString -> Action a ByteString

type Key = BS.ByteString

data State a b =
  State { getStateMaxSize :: Int
        , getStateTree :: SyncTree a
        , getStateSerializer :: b
        , getStateObsolete :: T.Trie Key (Chunk a)
        , getStateNew :: T.Trie Key (Chunk a) }

stateTree =
  L.lens getStateTree (\e v -> e { getStateTree = v })

stateSerializer =
  L.lens getStateSerializer (\e v -> e { getStateSerializer = v })

stateObsolete =
  L.lens getStateObsolete (\e v -> e { getStateObsolete = v })

stateNew =
  L.lens getStateNew (\e v -> e { getStateNew = v })

data Chunk a = Group { getGroupName :: Key
                     , getGroupHeight :: Key
                     , getGroupMembers :: M.Map Key (Chunk a)
                     , getGroupBody :: BS.ByteString }

             | Leaf { getLeafSerialized :: ByteString
                    , getLeafPath :: P.Path Key a }

data SyncTree a = SyncTree { getTreeByReverseKeyMember :: T.Trie Key (Chunk a)
                           , getTreeByHeightVacancy :: T.Trie Key (Chunk a) }

treeByReverseKeyMember =
  L.lens getTreeByReverseKeyMember
  (\t v -> t { getTreeByReverseKeyMember = v })

treeByHeightVacancy =
  L.lens getTreeByHeightVacancy (\t v -> t { getTreeByHeightVacancy = v })

leafKey = "l"

groupKey = "g"

empty = SyncTree Nothing T.empty T.empty

findObsolete chunk = do
  (T.find (byKey chunk) <$> get (treeByReverseKeyMember . stateTree))
    >>= \case
    Nothing -> patternFailure
    Just group -> do
      obsolete <- get stateObsolete
      if T.member path obsolete then
        return ()
        else
        update stateObsolete $ T.union (byHeightVacancy group)

        update stateNew
        $ T.union $ T.index byHeightVacancy $ groupMembers group

        findObsolete group

findObsoleteGroups = do
  get stateObsolete >>= mapM_ findObsolete
  obsolete <- get stateObsolete
  update stateNew (T.subtract obsolete)

groupTrie (Leaf _ _) _ = T.empty
groupTrie _ trie = trie

byKey chunk@(Leaf _ path) = P.super leafKey $ fmap (const chunk) path
byKey chunk@(Group name _ _ _) = P.super groupKey $ P.singleton name chunk

byReverseKeyMember chunk = groupTrie chunk $ T.index byKey $ groupMembers chunk

byHeightVacancy chunk =
  groupTrie chunk
  $ T.super (groupHeight chunk)
  $ byVacancy chunk

byVacancy chunk =
  groupTrie chunk
  $ T.super (groupVacancy chunk)
  $ T.singleton (groupName chunk) chunk

-- attempt to combine the most empty existing group with the most
-- empty new group to minimize fragmentation
combineVacant new old =
  case (,) <$> T.firstValue old <*> T.firstValue new of
    Nothing -> (T.empty, new)
    Just (oldFirst, newFirst) -> do
      combination <-
        makeGroup $ T.union (groupMembers oldFirst) (groupMembers newFirst)

      return (byVacancy oldFirst,
              T.union (byVacancy combination)
              $ T.subtract (byVacancy newFirst) new)

oneExactly [x] = Just x
oneExactly _ = Nothing

single = check . foldr (:) [] where
  check = \case
    [_] = True
    _ = False

twoOrMore = check . foldr (:) [] where
  check = \case
    _:_:_ = True
    _ = False

serialize' trie = with stateSerializer $ serialize trie

encrypt' text = with stateSerializer $ encrypt text

makeGroup members =
  T.firstValue members >>= fromChunks where
    fromChunks = \case
      (Leaf serialized _) -> fromLeaves serialized
      (Group _ height _ _) -> fromGroups height

    fromLeaves first = do
      plaintext <- oneExactly <$> serialize' (T.index leafPath members)

      let fragment = if single members then Just first else Nothing

      case plaintext <|> fragment of
        Nothing ->
          return Nothing

        Just text ->
          ciphertext <- encrypt' text
          return $ Just $ Group (C.hash [ciphertext]) 1 members ciphertext

    fromGroups height = do
      maxSize <- get stateMaxSize

      return do
        list <- collect 0 $ M.values members

        Just $ Group (C.hash list) (height + 1) members BS.empty where

          collect count = \case
            member:members ->
              let name = groupName member
                  count' = count a + BS.length name in

              if count' > maxSize then
                Nothing
              else
                (name:) <$> collect count' members

            _ -> Just []

addNewGroupsForHeight height =
  do
    new <- T.isolate (BS.pack $ show height) <$> get stateNew

    if height == 0 || twoOrMore new then
      -- todo: consider (psuedo)randomly inserting new leaves among
      -- existing and new groups.  Is there a way to do this that
      -- doesn't result in disproportionate network traffic?

      newGroups <- foldM group T.empty $ T.paths new

      obsolete <- T.sub above <$> get stateObsolete

      oldGroups <- T.subtract obsolete . T.sub above
                   <$> get (treeByHeightVacancy . stateTree)

      (newGroups', obsoleteGroups') <- combineVacant newGroups oldGroups

      update stateObsolete (T.union $ T.super above obsoleteGroups')
      update stateNew (T.union $ T.super above newGroups')
      else
      return ()
  where

    above = height + 1

    group groups ophan =
      case T.firstValue groups of
        Nothing ->
          return $ byHeightVacancy orphanGroup

        Just group ->
          makeGroup $ T.union orphan $ groupMembers group >>= \case
            Nothing ->
              orphanGroup <- makeGroup orphan

              return $ T.union
              (byHeightVacancy
               $ fromMaybe (exception "could not make a single-member group")
               orphanGroup) groups

            Just combined ->
              return
              $ T.union (byHeightVacancy combined)
              $ T.subtract (byHeightVacancy group) groups

addNewGroups =
  get stateNew >>= mapM_ addNewGroupsForHeight . T.keys

updateIndex index trie = do
  new <- get stateNew
  obsolete <- get stateObsolete

  return $ T.union (T.index index new)
    $ T.subtract (T.index index obsolete) trie

updateTree = do
  updateM (treeByReverseKeyMember . stateTree) (updateIndex byReverseKeyMember)
  updateM (treeByHeightVacancy . stateTree) (updateIndex byHeightVacancy)

split path =
  foldr visit (0, T.empty) <$> serialize' path where
    visit string (count, trie) =
      (count + 1, T.union (const (Leaf string path') <$> path') trie) where
        path' = T.super (BS.pack $ show count) path

toLeaves path = T.union $ T.super "0" <$> split path

update maxSize tree obsolete new = do
  obsoleteLeaves <- foldM toLeaves T.empty $ T.paths obsolete

  newLeaves <- foldM toLeaves T.empty $ T.paths new

  serializer <- S.get

  (_, state) <-
    try do
      findObsoleteGroups
      addNewGroups
      updateTree
    $ State maxSize tree serializer obsoleteLeaves newLeaves

  S.set (getStateSerializer state)

  return (getStateTree state,
          getStateObsolete state,
          getStateNew state)
