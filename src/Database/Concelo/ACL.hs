module Database.Concelo.ACL
  ( getListsBlackList
  , getListsWhiteList
  , getAclReadLists
  , getAclWriteLists
  , whiteList
  , whiteListAll
  , hash ) where

import qualified Database.Concelo.Set as S
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.Crypto as C
import qualified Data.ByteString as BS
import qualified Control.Lens as L

data ACL = ACL { getACLReadLists :: Lists
               , getACLWriteLists :: Lists
               , getACLHash :: ByteString }

aclReadLists =
  L.lens getAclReadLists (\x v -> x { getAclReadLists = v })

aclWriteLists =
  L.lens getAclWriteLists (\x v -> x { getAclWriteLists = v })

aclHash =
  L.lens getAclHash (\x v -> x { getAclHash = v })

data Lists = Lists { getListsBlackList :: S.Set BS.ByteString
                   , getListsWhiteList :: S.Set BS.ByteString }

listsBlackList =
  L.lens getListsBlackList (\x v -> x { getListsBlackList = v })

listsWhiteList =
  L.lens getListsWhiteList (\x v -> x { getListsWhiteList = v })

whiteList lens uid acl =
  setHash L.over lens (\lists -> Lists
                                 (S.delete uid $ getListsBlackList lists)
                                 (S.insert uid $ getListsWhiteList lists)) acl

whiteListAll lens uid acl =
  setHash $ L.over lens (\lists -> Lists
                                   S.empty
                                   (S.union $ getListsBlackList lists)) acl

isWriter key = S.member key . L.view (listsWhiteList . aclWriteLists)

permitNone trie =
  setHash $ ACL
  (Lists (T.keys $ T.sub P.aclReader trie) S.empty)
  (Lists (T.keys $ T.sub P.aclWriter trie) S.empty)

setHash acl = L.over aclHash (C.hash [readHash, writeHash]) acl where
  hashWhite = C.hash . getListsWhiteList
  readHash = hashWhite $ getACLReadLists acl
  writeHash = hashWhite $ getACLWriteLists acl

hash = getACLHash
