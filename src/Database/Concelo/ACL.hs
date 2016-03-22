{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Concelo.ACL
  ( ACL()
  , empty
  , getListsBlackList
  , getListsWhiteList
  , getACLReadLists
  , getACLWriteLists
  , writerKey
  , readerKey
  , whiteList
  , whiteListAll
  , permitNone
  , isWriter
  , hash ) where

import qualified Database.Concelo.Set as S
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Crypto as C
import qualified Data.ByteString as BS
import qualified Control.Lens as L

data ACL = ACL { getACLReadLists :: Lists
               , getACLWriteLists :: Lists
               , getACLHash :: BS.ByteString }

aclHash =
  L.lens getACLHash (\x v -> x { getACLHash = v })

data Lists = Lists { getListsBlackList :: S.Set BS.ByteString
                   , getListsWhiteList :: S.Set BS.ByteString }

whiteList lens uid acl =
  setHash $ L.over lens
  (\lists -> Lists
             (S.delete uid $ getListsBlackList lists)
             (S.insert uid $ getListsWhiteList lists)) acl

whiteListAll lens acl =
  setHash $ L.over lens
  (\lists -> Lists
             S.empty
             (S.union (getListsWhiteList lists) (getListsBlackList lists))) acl

isWriter key = S.member key . getListsWhiteList . getACLWriteLists

readerKey = "r"

writerKey = "w"

permitNone trie =
  setHash $ ACL
  (Lists (foldr S.insert S.empty $ T.keys $ T.sub readerKey trie) S.empty)
  (Lists (foldr S.insert S.empty $ T.keys $ T.sub writerKey trie) S.empty)
  undefined

empty =
  setHash $ ACL (Lists S.empty S.empty) (Lists S.empty S.empty) undefined

setHash acl = L.set aclHash (C.hash [readHash, writeHash]) acl where
  hashWhite = C.hash . getListsWhiteList
  readHash = hashWhite $ getACLReadLists acl
  writeHash = hashWhite $ getACLWriteLists acl

hash = getACLHash
