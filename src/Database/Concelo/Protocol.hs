module Database.Concelo.Protocol
  ( Message(Cred, Challenge, Published, Nack, Leaf, Group, Tree, Forest)
  , getTreeStream
  , getCredProtocolVersion
  , getChallengeProtocolVersion
  , aclWriter
  , aclReader
  , Value()
  , valueNumber
  , valueString
  , valueBoolean
  , valuePriority
  , rulesKey
  , version ) where

import Data.ByteString (ByteString)

import Database.Concelo.Control (character, prefix, (>>|), exec, eitherToMaybe,
                                 zeroOrMore, endOfStream)

import qualified Database.Concelo.Path as P
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Control as C
import qualified Database.Concelo.ACL as ACL
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
                    , getLeafTreeStream :: ByteString
                    , getLeafForestStream :: ByteString
                    , getLeafBody :: ByteString }

             | Group { getGroupName :: Name
                     , getGroupSigned :: Signed
                     , getLeafTreeStream :: ByteString
                     , getLeafForestStream :: ByteString
                     , getGroupMembers :: Names }

             | Tree { getTreeName :: Name
                    , getTreeStream :: ByteString
                    , getTreeForestStream :: ByteString
                    , getTreeOptional :: Bool
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

             | NoMessage

aclWriterKey = "w"

aclReaderKey = "r"

rulesKey = ".rules"

localVersion = (-1)

leafSize = 1024

treeLeafLevel = "0"

treeACLLevel = "1"

treeLevel = "2"

forestTreeLevel = "3"

forestACLLevel = "4"

forestLevel = "5"

toText = \case
  Leaf _ _ treeStream forestStream body ->
    return $ BS.concat [treeStream, forestStream, body]

  Group _ _ treeStream forestStream members ->
    return $ BS.concat [treeStream, forestStream, serializeNames members]

  Tree _ stream forestStream optional revision _ acl leaves ->
    return $
    BS.concat [stream, forestStream, if optional then "1" else "0",
               serializeInteger revision, serializeNames acl,
               serializeNames leaves]

  Forest _ stream revision adminRevision adminSigned acl trees ->
    return $
    BS.concat [stream, serializeInteger revision,
               serializeInteger adminRevision, getSignedSigner adminSigned,
               getSignedSignature adminSigned, getSignedText adminSigned,
               serializeNames acl, serializeNames trees]

  _ -> patternFailure

dummySigned = Signed BS.empty BS.empty BS.empty

dummyName = P.singleton ()

digest message (public, private) = do
  text <- toText message

  signature <- C.sign private text

  return (text, Signed public signature text)

leaf keyPair level treeStream forestStream body = do
  let leaf = Leaf dummyName dummySigned treeStream forestStream body

  (text, signed) <- digest leaf keyPair

  return leaf { getLeafName = P.super level
                              $ P.super "1"
                              $ P.singleton (C.hash [body]) ()

              , getLeafSigned = signed }

group keyPair level height treeStream forestStream members = do
  let group = Group dummyName dummySigned treeStream forestStream
              $ foldr (\member ->
                        T.union
                        $ P.super level
                        $ P.super (BS.pack $ show $ height - 1)
                        $ P.singleton member ()) T.empty members

  (text, signed) <- digest leaf keyPair

  return group { getGroupName = P.super level
                              $ P.super (BS.pack $ show height)
                              $ P.singleton (C.hash members) ()

               , getGroupSigned = signed }

tree (public, private) stream forestStream optional revision acl
  leaves = do
    let tree = Tree dummyName stream forestStream optional revision
               dummySigned acl leaves

    (text, signed) <- digest tree keyPair

    return tree { getTreeName = P.super treeLevel
                                $ P.singleton (C.hash [text]) ()

                , getTreeSigned = signed }

forest (public, private) stream revision adminRevision adminSigned acl trees =
  do
    let forest = Forest dummyName stream revision dummySigned adminRevision
                 adminSigned acl trees

    (text, signed) <- digest tree keyPair

    return forest { getForestName = P.super forestLevel
                                    $ P.singleton (C.hash [text]) ()

                  , getForestSigned = signed }

getMessageStreams = \case
  Leaf { getLeafTreeStream = ts, getLeafForestStream = fs } -> (ts, fs)
  Group { getGroupTreeStream = ts, getGroupForestStream = fs } -> (ts, fs)
  _ -> ""

serializeInteger = BS.concat $ writeInteger n []

writeInteger = \case
  0 -> (0:0:)
  n -> (case n .&. 255 of
           0 -> (0:1:)
           b -> (b:)) . writeInteger (n `shiftR` 8)

writeString s =
  let length = BS.length s in
  writeInteger length . (s:)

serializeNames = serializeTrie . fmap (const BS.empty)

serializeTrie trie = BS.concat $ writeTrie trie []

writeTrie trie =
  case TL.value trie of
    Nothing -> id
    Just v -> if null v then ("n":) else ("v":) . writeString v
  . TL.foldrPairs visit id trie where
    visit (k, a) = (("k":) . writeString k . writeTrie a . ("u":) .)

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
  (ord <$> character) >>= \case
    0 -> (ord <$> character) >>= \case
      0 -> return 0
      _ -> (`shiftL` 8) <$> size
    n -> ((+n) . (`shiftL` 8)) <$> size

stringOfSize size = do
  s <- get parseString
  if length s >= size then
    let (a, b) = splitAt size s in do
      set parseString b
      return a
    else
    noParse

key = do
  prefix "k"
  size >>= stringOfSize >>= \s -> update trieStatePath (s:)

path = oneOrMore key

value = (prefix "n" >> BS.empty) >>| (prefix "v" >> size >>= stringOfSize)

pathToValue =
  path >> value >>= \v -> do
    p <- get trieStatePath
    update trieStateTrie $ T.union $ P.toPath p $ v

up = do
  prefix "u"
  trieStatePath >>= \case
    [] -> error "stack underflow"
    (_, ks) -> set trieStatePath ks

done = prefix "d"

trie = zeroOrMore (pathToValue >>| up) >> done

serializeValue value = BS.concat $ writeValue value []

writeValue (Value _ priority _ body) =
  if priority /= defaultPriority then
    ("p":) . writeSize priority
  else
    id
  . case body of
    NullBody -> ("_":)
    NumberBody n -> ("n":) . writeString (show n)
    StringBody s -> ("s":) . writeString s
    BooleanBody True -> ("t":)
    BooleanBody False -> ("f":)

defaultPriority = 0

data ValueBody = NullBody
               | NumberBody Double
               | StringBody ByteString
               | BooleanBody Bool

data Value = Value { getValueSigner :: ByteString
                   , getValuePriority :: Int
                   , getValueACL :: ACL.ACL
                   , getValueBody :: ValueBody }

valueSigner =
  L.lens getValueSigner (\x v -> x { getValueSigner = v })

valuePriority =
  L.lens getValuePriority (\x v -> x { getValuePriority = v })

valueAcl =
  L.lens getValueAcl (\x v -> x { getValueAcl = v })

valueBody =
  L.lens getValueBody (\x v -> x { getValueBody = v })

valueNumber = \case
  Value { getValueBody = NumberBody n } -> Just n
  _ -> Nothing

valueString = \case
  Value { getValueBody = StringBody s } -> Just s
  _ -> Nothing

valueBoolean = \case
  Value { getValueBody = BooleanBody b } -> Just b
  _ -> Nothing

data ValueState = ValueState { getValueStateString :: ByteString
                             , getValueStateValue :: Value }

valueStateString =
  L.lens getValueStateString (\x v -> x { getValueStateString = v })

valueStateValue =
  L.lens getValueStateValue (\x v -> x { getValueStateValue = v })

instance C.ParseState ValueState where
  parseString = valueStateString

parseValue signer acl s =
  getValueStateValue
  <$> (eitherToMaybe $ exec value $ ValueState s
       $ Value signer defaultPriority acl NullBody)

priority = do
  prefix "p"
  size >>= set (valuePriority . valueStateValue)

nullBody =
  prefix "_"
  set (valueBody . valueStateValue) NullBody

-- todo: use an efficient binary encoding
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
