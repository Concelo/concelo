module SyncTree
  ( empty ) where

type Key = ByteString

class Serializer a where
  serialize :: a -> Int -> Trie b -> [ByteString]
  encrypt :: a -> ByteString -> (a, ByteString)

date Environment a b = Environment { _envTree :: SyncTree a
                                   , _envSerializer :: b
                                   , _envObsolete :: Trie Key (Chunk a)
                                   , _envNew :: Trie Key (Chunk a) }

data Chunk a = Group { groupName :: Key
                     , groupHeight :: Key
                     , groupMembers :: Trie Key (Chunk a) }
             | Leaf { leafSerialized :: ByteString
                    , leafPath :: Trie a }

data ChunkKey = GroupKey | LeafKey

data SyncTree a = SyncTree { _treeByReverseKeyMember :: Trie Key (Chunk a)
                           , _treeByHeightVacancy :: Trie Key (Chunk a) }

envTree = L.lens _envTree (\e v -> e { _envTree = v })
envSerializer = L.lens _envSerializer (\e v -> e { _envSerializer = v })
envObsolete = L.lens _envObsolete (\e v -> e { _envObsolete = v })
envNew = L.lens _envNew (\e v -> e { _envNew = v })

treeByReverseKeyMember = L.lens _treeByReverseKeyMember
                         (\t v -> t { _treeByReverseKeyMember = v })

treeByHeightVacancy = L.lens _treeByHeightVacancy
                      (\t v -> t { _treeByHeightVacancy = v })

empty = SyncTree Nothing T.empty T.empty

findObsolete chunk = do
  maybeGroup <- get (treeByReverseKeyMember . envTree) >>= T.find (byKey chunk)
  case maybeGroup of
    Nothing -> return ()
    Just group ->
      if T.member path obsolete then
        return () else
        update envObsolete $ T.union (byHeightVacancy group)
        update envNew
        $ T.union . (T.index byHeightVacancy $ groupMembers group)
        findObsolete group

findObsoleteGroups = do
  get envObsolete >>= mapM_ findObsolete
  obsolete <- get envObsolete
  update envNew (T.subtract obsolete)

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
combineVacant new old =
  case (,) <$> T.first old <*> T.first new of
    Nothing -> (T.empty, new)
    Just (oldFirst, newFirst) -> do
      combination <- makeGroup
                     $ T.union (groupMembers oldFirst) (groupMembers newFirst)

      return (byVacancy oldFirst,
              T.union (byVacancy combination)
              $ T.subtract (byVacancy newFirst) new)

oneExactly [x] = Just x
oneExactly _ = Nothing

serialize' trie =
  get envSerializer >>= flip serialize trie

encrypt' text = do
  serializer <- get envSerializer
  let (serializer', ciphertext) = encrypt serializer text
  set envSerializer serializer'
  return ciphertext

makeGroup members =
  return (T.first members >>= fromChunks) where
    fromChunks (List _ _) = fromLeaves
    fromChunks (Group _ _ _) = fromGroups
    
    fromLeaves = do
      plaintext <- serialize' $ T.index leafPath members >>= oneExactly

      case plaintext of
        Nothing -> return Nothing
        Just text ->
          ciphertext <- encrypt' text
          return $ Just $ Group (hash' ciphertext) 1 members ciphertext

    fromGroups = do
      serializer <- get envSerializer
      return do
        (height, count, body) <-
          foldr fold (Just (undefined, 0, BS.empty)) members

        Just $ Group (hash serializer body) (height + 1) members body where
        
          fold member (Just (_, count, string)) =
            if count > maxSize then
              Nothing else
              Just (groupHeight member,
                    count + BS.length name,
                    BS.append string name) where
                name = groupName member

          fold member Nothing = Nothing

addNewGroupsForHeight height =
  do
    new <- get envNew >>= return . T.isolate height
    if height == 0 || twoOrMore new then
      newGroups <- foldM group T.empty $ T.paths new
      oldGroups <- T.subtract obosolete . T.sub above
                   <$> get (treeByHeightVacancy . envTree)
                   <*> get envObsolete >>= T.sub above
                   
      (newGroups', obsoleteGroups') <- combineVacant newGroups oldGroups

      update envObsolete (T.union . T.super above obsoleteGroups')
      update envNew (T.union . T.super above newGroups') else
      return () where

  above = height + 1
          
  group groups ophan =
    case T.first groups of
      Nothing ->
        return $ byHeightVacancy orphanGroup
      Just group -> do
        maybeCombined <- makeGroup $ T.union orphan $ groupMembers group
        case maybeCombined of
          Nothing ->
            orphanGroup <- makeGroup orphan
            return $ T.union
            (byHeightVacancy 
             $ fromMaybe (error "could not make a single-member group")
             orphanGroup) groups
          Just combined ->
            return
            $ T.union (byHeightVacancy combined)
            $ T.subtract (byHeightVacancy group) groups

addNewGroups =
  get envNew >>= mapM_ addNewGroupsForHeight . T.keys

get lens =
  S.get >>= return $ L.view lens

set lens value =
  S.get >>= return $ L.set lens value >>= S.set

update lens state =
  get lens >>= state >>= set lens

updateIndex index trie = do
  new <- get envNew
  obsolete <- get envObsolete
  
  return $ T.add (T.index index new) $ T.subtract (T.index index obsolete) trie

updateTree = do
  update (treeByReverseKeyMember . envTree) (updateIndex byReverseKeyMember)
  update (treeByHeightVacancy . envTree) (updateIndex byHeightVacancy)

split serializer path =
  foldr fold (0, T.empty) $ serialize serializer path where
    fold string (count, trie) =
      (count + 1, T.union (fmap (const Leaf string path') path') trie) where
        path' = T.super count path

update tree serializer obsolete new =
  runState do
    findObsoleteGroups
    addNewGroups
    updateTree
    
    (,,,)
      <$> get envTree
      <*> get envSerializer
      <*> get envObsolete
      <*> get envNew

  $ Environment tree serializer
  (T.mapPaths toLeaves obsolete)
  (T.mapPaths toLeaves new) where
    toLeaves path = T.super 0 $ split serializer path
