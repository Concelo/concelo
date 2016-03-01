module Database.Concelo.Protocol
  ( Message(Cred, Challenge, Published)
  , getCredProtocolVersion
  , getChallengeProtocolVersion
  , version ) where

import Data.ByteString (ByteString)

import Database.Concelo.Control (character, prefix, (>>|), exec, eitherToMaybe)

import qualified Database.Concelo.Path as P
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Control as C
import qualified Data.ByteString as B

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
                    , getTreeStream :: Name
                    , getTreeOptional :: Bool
                    , getTreeKeyHash :: ByteString
                    , getTreeRevision :: Integer
                    , getTreeSigned :: Signed
                    , getTreeACL :: Name
                    , getTreeLeaves :: Name }

             | Forest { getForestName :: Name
                      , getForestStream :: Name
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

data TrieState =
  TrieState { getTrieStateString :: ByteString
            , getTrieStatePath :: [ByteString]
            , getTrieStateTrie :: T.Trie ByteString ByteString }

trieStateString =
  L.lens getTrieStateString (\x v -> x { getTrieStateString = v })

trieStatePath =
  L.lens getTrieStatePath (\x v -> x { getTrieStatePath = v })

trieStateTrie =
  L.lens getTrieStateTrie (\x v -> x { getTrieStateTrie = v })

instance C.TrieState TrieState where
  parseString = trieStateString

parseTrie s =
  getTrieStateTrie <$> (eitherToMaybe $ exec trie $ TrieState s [] T.empty)

size = do
  a <- ord <$> character
  b <- ord <$> character
  return (shiftL a 8) + b

stringOfSize size
  | size == 0 = return []
  | otherwise = character >>= \c -> (c:) <$> stringOfSize (size - 1)

key = do
  prefix 'k'
  size >>= stringOfSize >>= \s -> update trieStatePath (B.pack s :)

path = oneOrMore key

value = do
  prefix 'v'
  size >>= stringOfSize

pathToValue =
  path >> value >>= \v -> do
    p <- get trieStatePath
    update trieStateTrie $ T.union $ P.toPath p $ B.pack v

up = do
  prefix 'u'
  trieStatePath >>= \case
    [] -> error "stack underflow"
    (_, ks) -> set trieStatePath ks

done = prefix 'd'

trie = zeroOrMore (pathToValue >>| up) >> done

parseValue s = -- tbc

version = 0
