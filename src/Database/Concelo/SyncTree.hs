module SyncTree
  ( empty ) where

type Key = ByteString

class Serializer a where
  serialize :: a -> Trie b -> [ByteString]
  encrypt :: a -> ByteString -> (a, ByteString)

data Chunk a = Group { groupName :: Key
                     , groupHeight :: Key
                     , groupMembers :: Trie Key (Chunk a) }
             | Leaf { leafSerialized :: ByteString
                    , leafPath :: Trie a }

data ChunkKey = GroupKey | LeafKey

data SyncTree a = SyncTree { treeRoot :: Maybe Key
                           , treeByReverseKeyMember :: Trie Key (Chunk a)
                           , treeByHeightVacancy :: Trie Key (Chunk a) }

empty = SyncTree Nothing T.empty T.empty

findObsolete tree chunk result@(obsolete, new) =
  case T.find (byKey chunk) $ treeByReverseKeyMember tree of
    Nothing -> error "unable to find group containing chunk"
    Just group ->
      if T.member path obsolete then
        result else
        findObsolete tree group newResult where
          path = byHeightVacancy group
          newResult = (T.union path obsolete,
                       T.union (T.index byHeightVacancy
                                $ groupMembers group) new)

findObsoleteGroups tree result@(obsolete, new) =
  (obsolete', T.subtract obsolete' new') where
    (obsolete', new') = foldr (findObsolete tree) result obsolete

groupTrie (Leaf _ _) _ = T.empty
groupTrie _ trie = trie

byKey chunk@(Leaf _ path) = T.super LeafKey $ fmap (const chunk) path
byKey chunk@(Group name _ _) = T.super GroupKey $ T.singleton name chunk

byReverseKeyMember chunk =
  groupTrie chunk $ T.index index $ groupMembers chunk where
    index (Leaf _ path) = T.super LeafKey $ fmap (const chunk) path
    index (Group name _ _) = T.super GroupKey $ T.singleton name chunk

byHeightVacancy chunk =
  groupTrie chunk
  $ T.super (groupHeight chunk)
  $ byVacancy chunk

byVacancy chunk =
  groupTrie chunk
  $ T.super (groupVacancy chunk)
  $ T.singleton (groupName chunk)

-- attempt to combine the most empty existing group with the most
-- empty new group to minimize fragmentation
combineVacant serializer new old =
  fromMaybe (T.empty, new) do
    oldFirst <- T.first old
    newFirst <- T.first new
        
    (serializer', combination) <-
      makeGroup serializer
      $ T.union (groupMembers oldFirst) (groupMembers newFirst)

    Just (serializer',
          T.singleton (groupKey oldFirst) oldFirst,
          T.union (byVacancy combination) $ T.delete (groupKey newFirst) new)

head x:_ = Just x
head [] = Nothing

twoOrMore _:_:_ = True
twoOrMore _ = False

makeGroup serializer members =
  T.first members >>= fromChunks where
    fromChunks (List _ _) = fromLeaves
    fromChunks (Group _ _ _) = fromGroups
    
    fromLeaves = do
      (serializer', ciphertext) <- encrypt serializer =<<
        if twoOrMore strings then Nothing else head strings

      Just (serializer',
            Group (hash serializer ciphertext) 1 members ciphertext) where
        
        strings = serialize serializer $ T.index leafPath members

    fromGroups = do
      (height, count, body) <-
        foldr fold (Just (undefined, 0, BS.empty)) members

      Just (serializer,
            Group (hash serializer body) (height + 1) members body) where
        
        fold member (Just (_, count, string)) =
          if count > maxSize then
            Nothing else
            Just (groupHeight member,
                  count + BS.length name,
                  BS.append string name) where
              name = groupName member

        fold member Nothing = Nothing

-- todo: stop before creating a group with only one (non-leaf) member
addNewGroupsForHeight tree height (serializer, obsolete, new) =
  (serializer'',
   T.union (T.super above obsoleteGroups) obsolete,
   T.union (T.super above newGroups) new) where
    group orphan (serializer, groups) =
      case T.first groups of
        Nothing ->
          (serializer', byHeightVacancy orphanGroup)
        Just group ->
          case makeGroup $ T.union orphan $ groupMembers group of
            Nothing ->
              (serializer', T.union (byHeightVacancy orphanGroup) groups)
            Just (serializer', newGroup) ->
              (serializer', T.union (byHeightVacancy newGroup)
                            $ T.subtract (byHeightVacancy group) groups where
          (serializer', orphanGroup) =
            fromMaybe (error "could not make a single-member group")
            $ makeGroup serializer orphan

    above = height + 1

    (serializer', groups) =
      T.foldrPaths group (serializer, T.empty) $ T.isolate height new

    (serializer'', obsoleteGroups, newGroups) =
      combineVacant serializer' groups
      $ T.subtract (T.sub above obsolete)
      $ T.sub above
      $ treeByHeightVacancy tree

addNewGroups tree result@(_, _, new) =
  T.foldrKeys (addNewGroupsForHeight tree) result new

updateIndex trie index (obsolete, new) =
  T.add (T.index index new) $ T.subtract (T.index index obsolete) trie

apply tree result@(serializer, obsolete, new) =
  (tree { treeRoot = T.last new >>= Just . groupName
        , treeByReverseKeyMember =
          updateIndex (treeByReverseKeyMember tree) byReverseKeyMember result
        , treeByHeightVacancy =
          updateIndex (treeByHeightVacancy tree) byHeightVacancy result },
   serializer,
   obsolete,
   new)

split serializer path =
  foldr fold (0, T.empty) $ serialize serializer path where
    fold string (count, trie) =
      (count + 1, T.union (fmap (const Leaf string path') path') trie) where
        path' = T.super count path

update serializer tree obsolete new =
  (apply tree .
   addNewGroups tree .
   uncurry (serializer,,) .
   findObsoleteGroups tree)
  (T.mapPaths toLeaves obsolete,
   T.mapPaths toLeaves new) where
    toLeaves path = T.super 0 $ split serializer path
