module Database.Concelo.Protocol
  ( Message(Cred, Challenge, Published)
  , getCredProtocolVersion
  , getChallengeProtocolVersion
  , version ) where

import Data.ByteString (ByteString)

import qualified Database.Concelo.Path as P

type Name = P.Path ByteString ()

type Names = T.Trie ByteString ()

data Signed = Signed { getSignedSigner :: ByteString
                     , getSignedSignature :: ByteString
                     , getSignedText :: ByteString }

data Message = Cred { getCredProtocolVersion :: Int
                    , getCredRequest :: Integer
                    , getCredPublic :: ByteString
                    , getCredSignedChallenge :: ByteString }

             | Challenge { getChallengeProtocolVersion :: Int
                         , getChallengeBody :: ByteString }

             | Published { getPublishedForest :: Name }

             | Persisted { getPersistedForest :: Name }

             | Nack { getNackNames :: Names }

             | Leaf { getLeafName :: Name
                    , getLeafSigned :: Signed
                    , getLeafKeyHash :: ByteString
                    , getLeafBody :: ByteString }

             | Group { getGroupName :: Name
                     , getGroupSigned :: Signed
                     , getGroupKeyHash :: ByteString
                     , getGroupMembers :: Names }

             | Tree { getTreeName :: Name
                    , getTreeRevision :: Integer
                    , getTreeSigned :: Signed
                    , getTreeACL :: Name
                    , getTreeLeaves :: Name }

             | Forest { getForestName :: Name
                      , getForestRevision :: Integer
                      , getForestSigned :: Signed
                      , getForestAdminRevision :: Integer
                      , getForestAdminSigned :: Signed
                      , getForestACL :: Name
                      , getForestTrees :: Name }

getMessageKeyHash = \case
  Leaf { getLeafKeyHash = h } -> h
  Group { getGroupKeyHash = h } -> h
  _ -> ""

version = 0
