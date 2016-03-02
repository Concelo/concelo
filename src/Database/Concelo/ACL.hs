module Database.Concelo.ACL
  ( listsBlackList
  , listsWhiteList
  , aclReadLists
  , aclWriteLists
  , whiteList
  , whiteListAll ) where

import qualified Database.Concelo.Set as S
import qualified Data.ByteString as BS
import qualified Control.Lens as L

data ACL = ACL { getACLReadLists :: Lists
               , getACLWriteLists :: Lists }

aclReadLists =
  L.lens getAclReadLists (\x v -> x { getAclReadLists = v })

aclWriteLists =
  L.lens getAclWriteLists (\x v -> x { getAclWriteLists = v })

data Lists = Lists { getListsBlackList :: S.Set BS.ByteString
                   , getListsWhiteList :: S.Set BS.ByteString }

listsBlackList =
  L.lens getListsBlackList (\x v -> x { getListsBlackList = v })

listsWhiteList =
  L.lens getListsWhiteList (\x v -> x { getListsWhiteList = v })

whiteList lens uid acl =
  L.over lens (\lists -> Lists
                         (S.delete uid $ getListsBlackList lists)
                         (S.insert uid $ getListsWhiteList lists)) acl

whiteListAll lens uid acl =
  L.over lens (\lists -> Lists
                         S.empty
                         (S.union $ getListsBlackList lists)) acl
