module Database.Concelo.Ignis
  ( ignis ) where

import qualified Data.ByteString as BS
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Protocol as P

data Auth = Auth
  { authPrivate :: ByteString
  , authRequest :: Int }

data Ignis = Ignis
  { }

data Credentials
  = PrivateKey { privateKey :: ByteString }
  | EmailPassword { email :: Text, password :: Text }

ignis = Ignis

privateKeySizeInBytes = 32

authenticate ignis (PrivateKey private) =
  authenticateWithPrivateKey ignis private

authenticate ignis (EmailPassword email password) =
  authenticateWithPrivateKey ignis private
  $ C.derivePrivate privateKeySizeInBytes password email

authenticateWithPrivateKey ignis private =
  next $ maybeAuthenticate ignis
    { ignisAuth = Just $ Auth private (ignisNextRequest ingis) }

next ignis =
  (n, ignis { ignisNextRequest = n + 1 }) where n = ignisNextRequest ignis

maybeAuthenticate ignis =
  fromMaybe ignis $ liftA2 send (ignisChallenge ignis) (ignisAuth ignis) where
    
    send challenge auth =
      ignis { ignisPRNG = prng
            , ignisAuthMessage = Just message } where
        
        private = authPrivate auth

        (raw, prng) = C.sign (ignisPRNG ignis) private challenge

        message =
          P.serialize $ P.Auth (authRequest auth) (C.derivePublic private) raw

getAuth = C.derivePublic . ignisPrivate

unauth ignis = ignis { ignisAuth = Nothing }

update ignis update atomicUpdate =
  makeDiff ignis head atomicHead
  >>= \(ignis, diff) -> diff
    $>> obsoleteChunks ignis
    >>> newChunks ignis
    >>> todo
    >>> next
    >>> Right where
      head = transform $ ignisHead ignis
      atomicHead = atomicUpdate head

makeDiff ignis head atomicHead =
  diff >>= Right . (ignis { ignisHead = head }, ) where
    
    apply operation key value (obsolete, new) =
      if Access.writer (ignisID ignis) access (ignisBase ignis) key then
        
        Right case operation of
          T.Remove -> (T.insert key value obsolete, new)
          T.Add -> (obsolete,
                    T.insert key value { valueAccess = access } new) else
        
        Left "permission denied for update to " ++ toString key where
          access = Access.find key (ignisAccess ignis)

    diff = T.foldrDiff
           (\op key value result -> result >>= apply op key value)
           (Right (T.empty, T.empty))
           (ignisBase ignis)
           atomicHead

obsoleteChunks obsoleteValues newValues =
  foldr find (T.empty, newValues) obsoleteValues where
    find =
      findObsolete (Just . valueChunk) chunkKey chunkMembers obsoleteValues
  
-- attempt to combine the most empty existing chunk with the most
-- empty new chunk to minimize fragmentation
combineVacant new old =
  fromMaybe (T.empty, new) do
    oldFirst <- T.first old
    newFirst <- T.first new
        
    if T.twoOrMore combination then
      Nothing else
      Just (T.singleton (chunkKey oldFirst) oldFirst,
            T.union combination $ T.delete (chunkKey newFirst) new) where

        combination =
          makeChunks (chunkMembers oldFirst) (chunkMembers newFirst)

newChunksForAccess newValues oldChunks =
  combineVacant (T.foldrTrees makeNew T.empty newValues) oldChunks where

    access = valueAccess $ T.unsafeFirst newValues
      
    makeNew orphan result =
      case T.first result of
        Nothing ->
          makeChunks orphan
        
        Just chunk ->
          if T.twoOrMore new then
            T.union (makeChunks orphan) result else
            T.union new $ T.delete (chunkKey chunk) result where
              
              new = makeChunks $ T.union orphan $ chunkMembers chunk

newChunks ignis obsoleteChunks newValues =
  T.foldrChildren makeNewByAccess (obsoleteChunks, T.empty)
  newValuesByAccess where
    
    newValuesByAccess = T.groupBy valueAccess newValues

    makeNewByAccess newValues (obsoleteChunks, newChunks) =
      (T.union obsolete obsoleteChunks,
       T.set access new newChunks) where

        (obsolete, new) =
          newChunksForAccess newValues
          $ T.subtract obsoleteChunks
          $ T.descend access
          $ ignisChunks ignis

findObsolete itemContainer containerKey containerMembers obsoleteItems
  item result@(obsoleteContainers, newItems) =
    case itemContainer item of
      Nothing -> result
      Just container ->
        if T.member key obsoleteContainers then
          result else
          (T.insert key obsoleteContainers, T.union orphans newItems) where

            key = containerKey container
            orphans = T.difference (containerMembers container) obsoleteItems
              findObsolete groupParent groupKey groupMembers obsoleteGroups
  
obsoleteGroups obsoleteChunks newChunks =
  foldr find (T.empty, newChunks) obsoleteChunks
  where
    find' = findObsolete groupParent groupKey groupMembers obsoleteChunks
    
    find group result =
      find' group $ fromMaybe result (flip find result) $ groupParent group

newGroups newGroups =
  

  
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
