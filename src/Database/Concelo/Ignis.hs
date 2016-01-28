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
  = Anonymous
  | PrivateKey { privateKey :: ByteString }
  | EmailPassword { email :: Text, password :: Text }

ignis = Ignis

privateKeySizeInBytes = 32

anonymousKey =
  C.derivePrivate privateKeySizeInBytes "secret" "anonymous@example.com"

authenticate ignis Anonymous =
  authenticateWithPrivateKey ignis anonymousKey

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
            , ignisAuthMessage = Just message } } where
        
        private = authPrivate auth
        (raw, prng) = C.sign (ignisPRNG ignis) private challenge
        message =
          P.serialize $ P.Auth (authRequest auth) (C.derivePublic private) raw

getAuth = C.derivePublic . ignisPrivate

unauth ignis = ignis { ignisAuth = Nothing }

update ignis update atomicUpdate =
  next $ send ignis head atomicHead where
    head = transform $ ignisHead ignis
    atomicHead = atomicUpdate head

send ignis head atomicHead =
  either Left (Right . sendChunks ignis { ignisHead = head }) diff where
    
    foldDiff (T.Add key value) (removed, added) =
      if ACL.writer (ignisID ignis) acl then
        Right (removed, T.insert key value { valueACL = acl } added) else
        Left "permission denied for update to " ++ toString key where
          -- todo: intern ACLs for memory efficiency
          acl = ACL.make (ignisRules ignis) (ignisBase ignis) key

    foldDiff (T.Remove key value) (removed, added) =
      Right (T.insert key value removed, added)
      
    diff = T.foldDiff foldDiff (T.empty, T.empty) (ignisBase ignis) atomicHead

groupBy getKey insert empty map = foldr fold M.empty map where
  fold item result = M.insert key (insert item group) result where
    key = getKey item
    group = fromMaybe empty $ M.lookup key result

sendChunks ignis (removed, added) = where
  fold value result@(chunks, added) =
    if M.member name then
      (M.remove name, T.union added orphans) else
      result where
        chunk = valueChunk value
        name = chunkName chunk
        orphans = T.difference (chunkMembers chunk) removed
                                         
  (chunks, addedAndOrphans) =
    foldr fold ((ignisBaseChunks ingis), added) removed

  chunksByACL = groupBy chunkACL (M.insert =<< chunkName) M.empty chunks
  addedByACL = groupBy valueACL (T.insert =<< valueKey) T.empty addedAndOrphans

  fold' acl added result = where
    chunks = fromMaybe M.empty $ M.lookup acl chunksByACL
    -- tbc
  
-- 1. compute diff (as sets of removes and adds)
  
-- 2. remove chunks from base for each remove in diff and add orphans (any value that's in a removed chunk but not in the set of removes) to set of adds

-- (2.5.) find remaining chunk with the most space available
  
-- 3. if there's a remaining chunk into which the entire set of adds will fit, remove that chunk and create a new one with the union of orphans and adds
  
-- 4. otherwise, add a new chunk and fill it with as many adds as possible, then go back to 3, or go to 5 if no adds remain

-- 5. now we have a set of added chunks and a set of removed chunks.  Start again at 2, except at the level of chunks and groups of chunks.  Repeat this process (for groups of groups, groups of groups of groups, etc.) until the new root is calculated and collect everything that's new in reverse dependency order

-- NB: compute the new acks as the intersection of the set generated in 5 and old acks, and the new nacks as the difference between the two (i.e. new set minus new acks)

-- NB: if a nack is received (or an obsolete root), move the specified item from acks to nacks

-- NB: subscriber should send the last complete root it received periodically to ensure we're in sync

-- NB: each writer must segregate values into chunks according to permissions (i.e. all values in a given chunk must share the exact same read and write ACL).  However, groups may contain chunks with various ACLs.
