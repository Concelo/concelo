{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Concelo.ACL
  ( ACL()
  , empty
  , getListsBlackList
  , getListsWhiteList
  , getACLReadLists
  , getACLWriteLists
  , aclReadLists
  , aclWriteLists
  , writerKey
  , readerKey
  , Lists()
  , whiteList
  , whiteListEach
  , whiteListAll
  , fromBlackTrie
  , fromWhiteTrie
  , fromTries
  , toTrie
  , writerTrie
  , isWriter
  , hash ) where

import Database.Concelo.Control (eitherToAction)
import Database.Concelo.Misc (foldM)

import qualified Database.Concelo.Set as S
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Path as P
import qualified Database.Concelo.Crypto as C
import qualified Data.ByteString as BS
import qualified Control.Lens as L
import Prelude hiding (foldr)
import Data.Foldable (foldr)

data ACL = ACL { getACLReadLists :: Lists
               , getACLWriteLists :: Lists
               , getACLHash :: BS.ByteString }
           deriving Show

aclReadLists :: L.Lens' ACL Lists
aclReadLists =
  L.lens getACLReadLists (\x v -> x { getACLReadLists = v })

aclWriteLists :: L.Lens' ACL Lists
aclWriteLists =
  L.lens getACLWriteLists (\x v -> x { getACLWriteLists = v })

aclHash =
  L.lens getACLHash (\x v -> x { getACLHash = v })

data Lists = Lists { getListsBlackList :: S.Set BS.ByteString
                   , getListsWhiteList :: S.Set BS.ByteString }
             deriving Show

whiteList lens uid acl =
  setHash $ L.over lens
  (\lists -> Lists
             (S.delete uid $ getListsBlackList lists)
             (S.insert uid $ getListsWhiteList lists)) acl

whiteListEach lens uids acl =
  setHash $ L.over lens
  (\lists -> Lists
             (S.subtract uids $ getListsBlackList lists)
             (S.union uids $ getListsWhiteList lists)) acl

whiteListAll lens acl =
  setHash $ L.over lens
  (\lists -> Lists
             S.empty
             (S.union (getListsWhiteList lists) (getListsBlackList lists))) acl

isWriter key =
  S.member (C.fromPublic key) . getListsWhiteList . getACLWriteLists

readerKey = "r"

writerKey = "w"

fromBlackTrie blackTrie = fromTries T.empty blackTrie

fromWhiteTrie whiteTrie = fromTries whiteTrie T.empty

fromTries whiteTrie allTrie =
  setHash
  $ ACL (Lists readBlack readWhite) (Lists writeBlack writeWhite) undefined
  where
    readWhite = T.foldrKeys S.insert S.empty $ T.sub readerKey whiteTrie

    readBlack = S.subtract readWhite
                $ T.foldrKeys S.insert S.empty $ T.sub readerKey allTrie

    writeWhite = T.foldrKeys S.insert S.empty $ T.sub writerKey whiteTrie

    writeBlack = S.subtract writeWhite
                 $ T.foldrKeys S.insert S.empty $ T.sub writerKey allTrie

toTrie key acl = do
  r <- readers
  w <- writers

  return $ T.union (T.super readerKey r) (T.super writerKey w)

  where
    visitReader readers reader = do
      public <- eitherToAction $ C.toPublic reader
      encrypted <- C.encryptAsymmetric public (C.fromSymmetric key)

      return $ T.union (P.singleton reader encrypted) readers

    readers =
      foldM visitReader T.empty $ getListsWhiteList $ getACLReadLists acl

    writers =
      return
      $ foldr (\w -> T.union (P.singleton w BS.empty)) T.empty
      $ getListsWhiteList
      $ getACLWriteLists acl

empty =
  setHash $ ACL (Lists S.empty S.empty) (Lists S.empty S.empty) undefined

setHash acl = L.set aclHash (C.hash [readHash, writeHash]) acl where
  hashWhite = C.hash . getListsWhiteList
  readHash = hashWhite $ getACLReadLists acl
  writeHash = hashWhite $ getACLWriteLists acl

hash = getACLHash

writerTrie writers =
  T.union (T.super readerKey sub) (T.super writerKey sub) where
    sub = foldr (\a -> T.union (P.singleton a ())) T.empty writers
