module Database.Concelo.Protocol
  ( Message(Cred, Challenge, Published, Nack, Leaf, Group, Tree, Forest)
  , getTreeStream
  , getCredProtocolVersion
  , getChallengeProtocolVersion
  , version ) where

import Data.ByteString (ByteString)

import Database.Concelo.Control (character, prefix, (>>|), exec, eitherToMaybe,
                                 zeroOrMore, endOfStream)

import qualified Database.Concelo.Path as P
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Control as C
import qualified Control.Lens as L

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
                    , getTreeStream :: ByteString
                    , getTreeForestStream :: ByteString
                    , getTreeOptional :: Bool
                    , getTreeKeyHash :: ByteString
                    , getTreeRevision :: Integer
                    , getTreeSigned :: Signed
                    , getTreeACL :: Name
                    , getTreeLeaves :: Name }

             | Forest { getForestName :: Name
                      , getForestStream :: ByteString
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

instance C.ParseState TrieState where
  parseString = trieStateString

parseTrie s =
  getTrieStateTrie <$> (eitherToMaybe $ exec trie $ TrieState s [] T.empty)

size = do
  a <- ord <$> character
  b <- ord <$> character
  return (shiftL a 8) + b

stringOfSize size = do
  s <- get parseString
  if length s >= size then
    let (a, b) = splitAt size s in do
      set parseString b
      return a
    else
    noParse

key = do
  prefix 'k'
  size >>= stringOfSize >>= \s -> update trieStatePath (s :)

path = oneOrMore key

value = do
  prefix 'v'
  size >>= stringOfSize

pathToValue =
  path >> value >>= \v -> do
    p <- get trieStatePath
    update trieStateTrie $ T.union $ P.toPath p $ v

up = do
  prefix 'u'
  trieStatePath >>= \case
    [] -> error "stack underflow"
    (_, ks) -> set trieStatePath ks

done = prefix 'd'

trie = zeroOrMore (pathToValue >>| up) >> done

data ValueBody = NullBody
               | NumberBody Double
               | StringBody ByteString
               | BooleanBody Bool

data Value = Value { getValueSigner :: ByteString
                   , getValuePriority :: Int
                   , getValueBody :: ValueBody }

valueSigner =
  L.lens getValueSigner (\x v -> x { getValueSigner = v })

valuePriority =
  L.lens getValuePriority (\x v -> x { getValuePriority = v })

valueBody =
  L.lens getValueBody (\x v -> x { getValueBody = v })

data ValueState = ValueState { getValueStateString :: ByteString
                             , getValueStateValue :: Value }

valueStateString =
  L.lens getValueStateString (\x v -> x { getValueStateString = v })

valueStateValue =
  L.lens getValueStateValue (\x v -> x { getValueStateValue = v })

instance C.ParseState ValueState where
  parseString = valueStateString

parseValue signer s =
  getValueStateValue
  <$> (eitherToMaybe $ exec value $ ValueState s $ Value signer 0 NullBody)

priority = do
  prefix "p"
  size >>= set (valuePriority . valueStateValue)

nullBody =
  prefix "_"
  set (valueBody . valueStateValue) NullBody

numberBody =
  prefix "n"
  size
  >>= stringOfSize
  >>= set (valueBody . valueStateValue) . NumberBody . read

stringBody =
  prefix "s"
  size
  >>= stringOfSize
  >>= set (valueBody . valueStateValue) . StringBody

booleanBody = do
  (prefix "t" >>| prefix "f")
  >>= set (valueBody . valueStateValue) . BooleanBody . \case
    "t" -> True
    "f" -> False

value
  =  (optional priority)
  >> (nullBody >>| numberBody >>| stringBody >>| booleanBody)
  >> endOfStream

version = 0
