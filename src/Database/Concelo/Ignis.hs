module Database.Concelo.Ignis
  ( ignis ) where

import qualified Data.ByteString as BS
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Protocol as P

data Cred = Cred { _credPrivate :: ByteString
                 , _credPublic :: ByteString
                 , _credRequest :: Int
                 , _credSent :: Bool }

data Ignis = Ignis { }

data Credentials = PrivateKey { _privateKey :: ByteString }
                 | EmailPassword { _email :: Text
                                 , _password :: Text }

ignis rules = R.parse rules >>= Right . Ignis

authenticate (PrivateKey private) =
  authenticateWithPrivateKey private

authenticate (EmailPassword email password) =
  authenticateWithPrivateKey $ C.derivePrivate password email

authenticateWithPrivateKey private =
  request <- get ignisNextRequest
  set ignisCred $ Just $ Cred private (C.derivePublic private) request False
  nextRequest

nextRequest = do
  getThenUpdate ignisNextRequest (+1)

with lens f =
  (result, new) <- get lens >>= f
  set lens new
  return result

sign private challenge =
  with ignisPRNG $ C.sign private challenge

bind f x =
  x >>= \case
    Nothing -> return Nothing
    Just x' -> f x'

bind2 f x y =
  x >>= \case
    Nothing -> return Nothing
    Just x' -> y >>= \case
      Nothing -> return Nothing
      Just y' -> f x' y'

maybeAuthenticate =
  bind2 try (get ignisCred) (get ignisChallenge) where
      
    try cred challenge =
      if getCredSent cred then return Nothing else do
        set ignisCred $ Just $ L.set credSent True cred
        
        sign (getCredPrivate cred) challenge
          >>= P.serialize
          . P.Cred (getCredRequest cred) (getCredPublic cred)

getPublic =
  get ignisCred >>= \case
    Nothing -> return Nothing
    Just cred -> return $ getCredPublic cred

unAuth = set ignisCred Nothing

update now update atomicUpdate = do
  head <- fmap update $ get ignisHead
  let atomicHead = atomicUpdate head
  makeDiff head atomicHead >>= set ignisDiff
  set ignisUpdateTime now      
  set ignisHead head

maybeUpdateForest lens hash sequence = do
  get lens >>= \case
    Nothing ->
      set lens $ Just $ Forest hash sequence

    Just forest@(Forest _ latest) ->
      if sequence > latest then
        set lens $ Just
        $ L.set forestSequence sequence
        $ L.set forestHash hash forest else
        return ()
        
  checkForests

isComplete hash = fmap (null . T.sub hash) (get ignisMissingByGroup)

fail = throwError Failure

error = throwError . Error

updateCurrent current latest =
  get latest >>= \case
    Nothing -> return ()
    maybeLatest@(Just (Forest latestHash latestSequence)) ->
      get current >>= \case
        Nothing ->
          ifM (isComplete latestHash)
          (ifM (isAuthentic latestHash)
           (set current maybeLatest >> return (Nothing, latestHash))
           (set latest Nothing >> fail))
          fail
          
        maybeCurrent@(Just (Forest currentHash currentSequence)) ->
          ifM (return (latestSequence > currentSequence)
               `andM` (complete latestHash))
          (ifM (isAuthentic latestHash
                `andM` (latestHash `isNewerThan` currentHash))
           (set current maybeLatest >> return (Just currentHash, latestHash))
           (set latest maybeCurrent >> fail))
          fail

updateUnsanitized (maybeOldHash, newHash) = do
  received <- get ignisReceived
  case T.find newHash received of
    Just (P.Forest {}) ->
      case oldNash >>= flip T.find received of
        Nothing -> foo
    _ -> error "could not find forest for hash"

    

checkForests = do
  try $ updateCurrent ignisCurrentPersisted ignisLatestPersisted
    >>= notifyCallbacks

  try $ updateCurrent ignisCurrentPublished ignisLatestPublished
    >>= updateUnsanitized
    >>= updateBase
    >>= mergeHead
    >>  cleanCaches -- remove obsolete chunks from "received" and "missing" tries
    >>  notifyHeadChanged -- request call to update so we can publish head with the latest atomic updates

addMissingToGroups member group (byMember, byGroup) =
  foldr (addMissingToGroups member) result $ T.sub group byMember where
    result = (T.insert (T.super member $ T.value group group) byMember,
              T.insert (T.super group $ T.value member ()) byGroup)

addMissing group members = do
  received <- get ignisReceived
  byMember <- get ignisMissingByMember
  byGroup <- get ignisMissingByGroup
  
  let fold member result@(byMember, byGroup) = case T.find member received of
        Nothing -> addMissingToGroups member group result
          
        Just (P.Group { P.getGroupMembers = members }) ->
          foldr fold result members
          
        Just _ -> result

      (byMember', byGroup') = foldr fold (byMember, byGroup) members

  set ignisMissingByMember byMember'
  set ignisMissingByGroup byGroup'

updateMissing member = do
  byMember <- getAndUpdate ignisMissingByMember $ T.remove member
        
  update ignisMissingByGroup \byGroup ->
    foldr fold byGroup $ T.sub member byMember where
      fold group = T.subtract $ T.super group $ T.value member ()

isComplete name =
  fmap (null . T.sub name) (get $ ignisMissing . missingByGroup)

resetReceiver = get ignisCleanReceiver >>= set ignisReceiver >> fail

findNewChunks oldChunks newChunks newRoot =
  visit newRoot (T.empty, T.empty) where
    visit name (new, found) =
      case M.find name oldChunks of
        Just chunk -> return (new, M.insert name chunk found)
            
        Nothing -> find name newChunks >>= \chunk ->
          foldM visit (M.insert name chunk new, found) (chunkMembers chunk)

findObsoleteChunks oldChunks oldRoot found =
  visit oldRoot T.empty where
    visit name obsolete =
      if M.member name found then
        obsolete else
        find name oldChunks >>= \chunk ->
          foldM visit (M.insert name chunk obsolete) (chunkMembers chunk)

diffChunks oldChunks oldRoot newChunks newRoot = do
  (new, found) <- findNewChunks oldChunks newChunks newRoot
  obsolete <- findObsoleteChunks newChunks oldRoot found
  return (obsolete, new)

updateForest newName current = do
  received <- get ignisReceived
  case T.find newName received of
    Just P.Forest { P.getForestRevision = newRevision
                  , P.getForestACL = newACL } ->
      if revision <= getForestRevision current then
        resetReceiver else
        diffChunks (getForestChunks current) (getForestACL current) received newACL
        >>= todo
      
    _ -> error "forest not found"

checkForest lens name = try do
  assert $ isComplete name
  get lens >>= updateForest name >>= set lens

checkIncomplete =
  get ignisIncomplete >>= mapM_ \case
    P.Persisted name -> checkForest ignisPersisted name
    P.Published name -> checkForest ignisPublished name
    _ -> error "unexpected message in ignisIncomplete"

receiveChunk chunk name members = do
  update ignisReceived $ T.insert name chunk
  addMissing name members
  removeMissing name
  checkIncomplete

receive = \case
  P.Challenge challenge -> do
    set ignisChallenge $ Just challenge
    get ignisCred >>= \case
      Nothing -> return ()
      Just cred -> set ignisCred $ Just $ L.set credSent False cred

  leaf@(P.Leaf { P.getLeafName = name }) ->
    receiveChunk leaf name []

  group@(P.Group { P.getGroupName = name
                 , P.getGroupMembers = members }) ->
    receiveChunk group name members

  tree@(P.Tree { P.getTreeName = name
               , P.getTreeACL = acl }) ->
    receiveChunk tree name [acl]

  forest@(P.Forest { P.getForestName = name
                   , P.getForestTrees = trees
                   , P.getForestACL = acl }) ->
    receiveChunk forest name [trees, acl]

  p@(P.Persisted forest) -> do
    update ignisIncomplete $ T.insert (T.super PersistedKey $ T.value forest p)
    checkIncomplete

  p@(P.Published forest) -> do
    update ignisIncomplete $ T.insert (T.super PublishedKey $ T.value forest p)
    checkIncomplete

  P.Nack path -> do
    fmap (T.findPath path) (get ignisAcks) >>= \case
      Just nack -> do
        update ignisNacks $ T.union nack
        update ignisAcks $ T.subtract nack
        return ()

      Nothing -> return ()

nextMessage =
  get ignisCred >>= \case
    Nothing -> return Nothing
    Just _ ->
      maybeAuthenticate >>= \case
        Just message ->
          return $ Just message

        Nothing -> do
          maybeUpdateTrees
      
          fmap T.firstPath (get ignisNacks) >>= \case
            Just nack -> do
              update ignisNacks $ T.subtract nack
              update ignisAcks $ T.union nack
              return $ T.first nack

            Nothing -> return Nothing
            -- todo: request missing
            -- todo: send current root periodically

mapPair f (x, y) = (f x, f y)

byACL path value = T.super (getValueACL value) path

withSerializer constructor f =
  ignisCred >>= \case
    Nothing -> error "cannot encrypt without private key"
    Just cred -> 
      with ignisPRNG (f . constructor (getCredPrivate cred))

updateACLTrees (obsoleteValues, newValues) = do
  with ignisACLTrees \trees ->
    withSerializer ValueSerializer \serializer ->
      foldr update (((T.empty, T.empty, T.empty, T.empty), trees), serializer)
            $ T.union (T.keys obsoleteValues) (T.keys newValues) where
    
    update acl (((allObsolete, allNew, obsoleteTrees, newTrees),
                 trees), serializer) =
      
      (((T.union obsolete' allObsolete,
         T.union new' allNew,
         
         if ST.null (getAnnotatedValue tree) then
           obsoleteTrees else
           T.insert acl tree obsoleteTrees,
         
         if ST.null tree' then
           newTrees else
           T.insert acl (Annotated tree' acl) newTrees),
        
        if ST.null tree' then
          T.delete acl trees else
          T.insert acl tree' trees),
       
       serializer') where
        
        tree = fromMaybe ST.empty $ T.find (T.key acl) trees
        
        (tree', serializer', obsolete', new') =
          ST.update (annotatedValue tree) serializer
          (T.sub acl obsoleteValues) (T.sub acl newValues)

whoAmI = fromMaybe $ error "I don't know who I am!"

updateMyTree (allObsolete, allNew, obsoleteTrees, newTrees) = do
  cred <- get ignisCred >>= whoAmI
  challenge <- get ignisChallenge >>= whoAmI
  revision <- getThenUpdate ignisNextRevision (+1)
  time <- get ignisUpdateTime
   -- all trees from other writers using the same keypair we're using
   -- except for the most recent published revision, if applicable:
  obsoleteTrees <- get ignisPeerTrees

  with ignisMyTree \tree ->
    -- NB: the acl serializer will enumerate the key encrypted for all authorized readers
    withSerializer ACLTreeSerializer \serializer ->
      let (tree', serializer', obsolete', new') =
            ST.update (annotatedValue tree) serializer obsoleteTrees
            newTrees
          annotated = (Annotate tree'
                       (Stamp
                        (getIgnisPublic cred) challenge revision time)) in
      (((T.union obsolete' allObsolete,
         T.union new' allNew,
      
         if ST.isEmpty (getAnnotatedValue tree) then
           obsoleteTrees else
           T.insert key tree obsoleteTrees,
      
         if ST.isEmpty tree' then
           T.empty else
           T.singleton revision annotated),
        
        annotated),
       serializer')

updateRootTree (allObsolete, allNew, obsoleteTrees, newTrees) = do
  with ignisRootTree \tree ->
    withSerializer WriterTreeSerializer \serializer ->
      let (tree', serializer', obsolete', new') =
            ST.update tree serializer obsoleteTrees newTrees in
      (((T.union obsolete' allObsolete,
         T.union new' allNew),
        tree'),
       serializer')

updateAcksAndNacks (allObsolete, allNew) = do
  acks <- updateThenGet ignisAcks $ T.intersect allNew . T.subtract allObsolete
  update ignisNacks T.union (T.subract acks allNew) . T.subtract allObsolete

maybeUpdateTrees =
  get ignisDiff >>= \case
    Nothing -> return ()
    Just diff ->
      updateACLTrees $ mapPair (T.indexPaths byACL) diff
      >>= updateMyTree
      >>= updateRootTree
      >>= updateAcksAndNacks

data Context = Context { getContextMe :: PublicKey
                       , getContextRevision :: Integer
                       , getContextRoot :: Trie Key (Value a)
                       , getContextEnv :: Trie Key Key
                       , getContextBase :: Trie Key (Value a)
                       , getContextHead :: Trie Key (Value a) }

updateACL context acl acls rules =
  if (getRulesValid rules) context then
    Right $ intern
    (L.over aclWrite ((getRulesWrite rules) context)
     $ L.over aclRead ((getRulesRead rules) context) acl) acls else
    Left "validation failed"

validate context rules diff path acl
  result@(tries@(obsoleteTrie, newTrie), acls) =
    (if T.empty diff then
       result else
       case fromMaybe (Right (acl, acls))
            (T.value rules >>= Just $ updateACL context acl acls) of
         Right (acl', acls') ->
           case T.value diff of
             Just (obsoleteValue, newValue) ->
               if aclWriter (getContextMe context) acl' then
                 Right $ foldr fold
                 ((maybe obsoleteTrie \v ->
                     T.union (singleton path v) obsoleteTrie,
                
                   maybe newTrie \v ->
                     T.union (singleton path $ L.set valueACL acl' v) newTrie),
                  
                  acls')
                 $ T.keys diff else
                 Left "write access denied"

             Nothing -> Right foldr fold (tries, acls') $ T.keys diff
      
         Left error -> Left error) where
    
      fold key result =
        let rules' = T.sub key rules
            context' = L.over contextBase (T.sub key)
                       $ L.over contextHead (T.sub key)
            diff' = T.sub key diff
            path' = key : path
            result' = validate context' rules' diff' path' acl result in
      
        if T.null rules' then
          case T.value rules >>= getRulesWildCard of
            Just (name, rules') ->
              validate (L.over contextEnv (T.insert name key) context') rules'
              diff' path' acl result
            
            Nothing ->
              result' else
          result'

makeDiff head = do
  me <- getPublic >>= whoAmI
  base <- get ignisBase
  rules <- get ignisRules
  revision <- get ignisNextRevision
  emptyACL <- get ignisEmptyACL
  
  with ignisACLs \acls ->
    validate (Context me revision base T.empty base head) rules
    (T.diff base head) [] emptyACL ((T.empty, T.empty), acls)

validUpdate obsoletePath newPath = do
  me <- getPublic >>= whoAmI
  base <- get ignisBase
  valid <- get ignisValid
  if valid obsoletePath newPath base me then do
    readWrite <- get ignisReadWrite
    trees <- get ignisACLTrees
    let acl = readWrite obsoletePath newPath base trees
    if writer acl me then
      return $ Right acl else
      return $ Left "write access denied" else
    return $ Left "validation failed"

makeDiff head atomicHead = do
  base <- get ignisBase
  foldM apply (Right (T.empty, T.empty)) $ T.diff base atomicHead where

    apply (obsoletePath, newPath) diff = do
      acl <- validateUpdate obsoletePath newPath
      return (apply' <$> diff <*> acl) where
    
        apply' (obsoletePaths, newPaths, newACLs) acl =
          (T.union obsoletePath obsoletePaths,
           T.union (fmap (L.set valueACL acl) newPath) newPaths)

-- 1. compute diff (as sets of removes and adds)
  
-- 2. remove chunks from base for each remove in diff and add orphans (any value that's in a removed chunk but not in the set of removes) to set of adds

-- (2.5.) find remaining chunk with the most space available
  
-- 3. if there's a remaining chunk into which the entire set of adds will fit, remove that chunk and create a new one with the union of orphans and adds
  
-- 4. otherwise, add a new chunk and fill it with as many adds as possible, then go back to 3, or go to 5 if no adds remain

-- 5. now we have a set of added chunks and a set of removed chunks.  Start again at 2, except at the level of chunks and groups of chunks.  Repeat this process (for groups of groups, groups of groups of groups, etc.) until the new root is calculated and collect everything that's new in reverse dependency order

-- NB: compute the new acks as the intersection of the set generated in 5 and old acks, and the new nacks as the difference between the two (i.e. new set minus new acks)

-- NB: if a nack is received (or an obsolete root), move the specified item from acks to nacks

-- NB: subscriber should send the last complete root it received periodically to ensure we're in sync

-- NB: each writer must segregate values into chunks according to permissions (i.e. all values in a given chunk must share the exact same read and write Access).  However, groups may contain chunks with various Accesses.
