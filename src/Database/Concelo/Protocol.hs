{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Concelo.Protocol
  ( Message(Cred, Challenge, Published, Persisted, Nack, Leaf, Group, Tree,
            Forest, NoMessage)
  , Signed(Signed)
  , Name
  , Names
  , getSignedSigner
  , getSignedSignature
  , getSignedText
  , getCredProtocolVersion
  , getCredRequest
  , getCredPublic
  , getCredSignedChallenge
  , getChallengeProtocolVersion
  , getChallengeBody
  , getPublishedForest
  , getPersistedForest
  , getNackNames
  , getLeafName
  , getLeafSigned
  , getLeafTreeStream
  , getLeafForestStream
  , getLeafBody
  , getGroupName
  , getGroupSigned
  , getGroupTreeStream
  , getGroupForestStream
  , getGroupMembers
  , getTreeName
  , getTreeStream
  , getTreeForestStream
  , getTreeOptional
  , getTreeRevision
  , getTreeSigned
  , getTreeACL
  , getTreeLeaves
  , getForestName
  , getForestStream
  , getForestRevision
  , getForestSigned
  , getForestAdminRevision
  , getForestAdminSigned
  , getForestACL
  , getForestTrees
  , aclWriterKey
  , aclReaderKey
  , rulesKey
  , localVersion
  , leafSize
  , treeLeafLevel
  , treeACLLevel
  , treeLevel
  , forestTreeLevel
  , forestACLLevel
  , forestLevel
  , Value()
  , getValueSigner
  , getValuePriority
  , getValueACL
  , getValueBody
  , valuePriority
  , valueNumber
  , valueString
  , valueBoolean
  , valueSigner
  , valueACL
  , value
  , serializeValue
  , serializeTrie
  , parseValue
  , parseTrie
  , leaf
  , group
  , tree
  , forest
  , version ) where

import Database.Concelo.Control (prefix, (>>|), exec, eitherToMaybe,
                                 zeroOrOne, zeroOrMore, oneOrMore, endOfStream,
                                 patternFailure, get, set, update, noParse,
                                 exception, bsShow, bsRead)

import qualified Database.Concelo.Path as P
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.TrieLike as TL
import qualified Database.Concelo.Control as Co
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.ACL as ACL
import qualified Database.Concelo.Bytes as B
import qualified Control.Lens as L
import qualified Data.ByteString.Char8 as BS

type Name = P.Path BS.ByteString ()

type Names = T.Trie BS.ByteString ()

data Signed = Signed { getSignedSigner :: Cr.PublicKey
                     , getSignedSignature :: BS.ByteString
                     , getSignedText :: BS.ByteString }

data Message = Cred { getCredProtocolVersion :: Int
                    , getCredRequest :: Integer
                    , getCredPublic :: BS.ByteString
                    , getCredSignedChallenge :: BS.ByteString }

             | Challenge { getChallengeProtocolVersion :: Int
                         , getChallengeBody :: BS.ByteString }

             | Published { getPublishedForest :: Name }

             | Persisted { getPersistedForest :: Name }

             | Nack { getNackNames :: Names }

             | Leaf { getLeafName :: Name
                    , getLeafSigned :: Signed
                    , getLeafTreeStream :: BS.ByteString
                    , getLeafForestStream :: BS.ByteString
                    , getLeafBody :: BS.ByteString }

             | Group { getGroupName :: Name
                     , getGroupSigned :: Signed
                     , getGroupTreeStream :: BS.ByteString
                     , getGroupForestStream :: BS.ByteString
                     , getGroupMembers :: Names }

             | Tree { getTreeName :: Name
                    , getTreeStream :: BS.ByteString
                    , getTreeForestStream :: BS.ByteString
                    , getTreeOptional :: Bool
                    , getTreeRevision :: Integer
                    , getTreeSigned :: Signed
                    , getTreeACL :: Name
                    , getTreeLeaves :: Name }

             | Forest { getForestName :: Name
                      , getForestStream :: BS.ByteString
                      , getForestRevision :: Integer
                      , getForestSigned :: Signed
                      , getForestAdminRevision :: Integer
                      , getForestAdminSigned :: Signed
                      , getForestACL :: Name
                      , getForestTrees :: Name }

             | NoMessage

aclWriterKey = ACL.writerKey

aclReaderKey = ACL.readerKey

rulesKey = ".rules" :: BS.ByteString

localVersion = (-1) :: Integer

leafSize = 1024 :: Int

treeLeafLevel = "0" :: BS.ByteString

treeACLLevel = "1" :: BS.ByteString

treeLevel = "2" :: BS.ByteString

forestTreeLevel = "3" :: BS.ByteString

forestACLLevel = "4" :: BS.ByteString

forestLevel = "5" :: BS.ByteString

toText = \case
  Leaf _ _ treeStream forestStream body ->
    return $ BS.concat [treeStream, forestStream, body]

  Group _ _ treeStream forestStream members ->
    return $ BS.concat [treeStream, forestStream, serializeNames members]

  Tree _ stream forestStream optional revision _ acl leaves ->
    return $
    BS.concat [stream, forestStream, if optional then "1" else "0",
               B.fromInteger revision, serializeName acl,
               serializeName leaves]

  Forest _ stream revision _ adminRevision adminSigned acl trees ->
    return $
    BS.concat [stream, B.fromInteger revision,
               B.fromInteger adminRevision,
               Cr.fromPublic $ getSignedSigner adminSigned,
               getSignedSignature adminSigned, getSignedText adminSigned,
               serializeName acl, serializeName trees]

  _ -> patternFailure

dummySigned = Signed undefined undefined undefined

dummyName = P.leaf ()

digest message (public, private) = do
  text <- toText message

  signature <- Cr.sign private text

  return (text, Signed public signature text)

leaf keyPair level treeStream forestStream body = do
  let leaf = Leaf dummyName dummySigned treeStream forestStream body

  (_, signed) <- digest leaf keyPair

  return leaf { getLeafName = P.super level
                              $ P.super "1"
                              $ P.singleton (Cr.hash [body]) ()

              , getLeafSigned = signed }

group :: Foldable t =>
         (Cr.PublicKey, Cr.PrivateKey) ->
         Int ->
         Int ->
         BS.ByteString ->
         BS.ByteString ->
         t BS.ByteString ->
         Co.Action Cr.PRNG Message
group keyPair level height treeStream forestStream members = do
  let group = Group dummyName dummySigned treeStream forestStream
              $ foldr (\member ->
                        T.union
                        $ P.super (bsShow level)
                        $ P.super (bsShow $ height - 1)
                        $ P.singleton member ()) T.empty members

  (_, signed) <- digest group keyPair

  return group { getGroupName = P.super (bsShow level)
                              $ P.super (bsShow height)
                              $ P.singleton (Cr.hash members) ()

               , getGroupSigned = signed }

tree keyPair stream forestStream optional revision acl
  leaves = do
    let tree = Tree dummyName stream forestStream optional revision
               dummySigned acl leaves

    (text, signed) <- digest tree keyPair

    return tree { getTreeName = P.super treeLevel
                                $ P.singleton (Cr.hash [text]) ()

                , getTreeSigned = signed }

forest keyPair stream revision adminRevision adminSigned acl trees =
  do
    let forest = Forest dummyName stream revision dummySigned adminRevision
                 adminSigned acl trees

    (text, signed) <- digest forest keyPair

    return forest { getForestName = P.super forestLevel
                                    $ P.singleton (Cr.hash [text]) ()

                  , getForestSigned = signed }

writeString :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
writeString s = (B.fromInteger (BS.length s) :) . (s :)

serializeNames :: Names -> BS.ByteString
serializeNames = serializeTrie . fmap (const BS.empty)

serializeName :: Name -> BS.ByteString
serializeName = serializeNames . flip T.union T.empty

serializeTrie :: T.Trie BS.ByteString BS.ByteString -> BS.ByteString
serializeTrie trie = BS.concat $ writeTrie trie []

writeTrie :: T.Trie BS.ByteString BS.ByteString ->
             [BS.ByteString] ->
             [BS.ByteString]
writeTrie trie =
  case TL.value trie of
    Nothing -> id
    Just v -> if BS.null v then ("n":) else ("v":) . writeString v
  . TL.foldrPairs visit id trie where
    visit (k, a) = ((("k":) . writeString k . writeTrie a . ("u":)) .)

data TrieState =
  TrieState { getTrieStateString :: BS.ByteString
            , getTrieStatePath :: [BS.ByteString]
            , getTrieStateTrie :: T.Trie BS.ByteString BS.ByteString }

trieStateString :: L.Lens' TrieState BS.ByteString
trieStateString =
  L.lens getTrieStateString (\x v -> x { getTrieStateString = v })

trieStatePath :: L.Lens' TrieState [BS.ByteString]
trieStatePath =
  L.lens getTrieStatePath (\x v -> x { getTrieStatePath = v })

trieStateTrie =
  L.lens getTrieStateTrie (\x v -> x { getTrieStateTrie = v })

instance Co.ParseState TrieState where
  parseString = trieStateString

parseTrie s =
  getTrieStateTrie <$> (eitherToMaybe $ exec trie $ TrieState s [] T.empty)

stringOfSize :: Co.ParseState s => Integer -> Co.Action s BS.ByteString
stringOfSize size = do
  s <- get Co.parseString
  let isize = fromIntegral size
  if BS.length s >= isize then
    let (a, b) = BS.splitAt isize s in do
      set Co.parseString b
      return a
    else
    noParse

key :: Co.Action TrieState ()
key = do
  prefix "k"
  B.toInteger >>= stringOfSize >>= \s -> update trieStatePath (s:)

path = oneOrMore key

parseLeaf
  =   (prefix "n" >> return BS.empty)
  >>| (prefix "v" >> B.toInteger >>= stringOfSize)

pathToValue =
  path >> parseLeaf >>= \v -> do
    p <- get trieStatePath
    update trieStateTrie $ T.union $ P.toPath p v

up = do
  prefix "u"
  get trieStatePath >>= \case
    [] -> exception "stack underflow"
    _:ks -> set trieStatePath ks

done = prefix "d"

trie = zeroOrMore (pathToValue >>| up) >> done >> return ()

serializeValue :: Value -> BS.ByteString
serializeValue value = BS.concat $ writeValue value []

writeValue :: Value -> [BS.ByteString] -> [BS.ByteString]
writeValue (Value _ priority _ body) =
  if priority /= defaultPriority then
    ("p":) . (B.fromInteger priority :)
  else
    id
  . case body of
    NullBody -> ("_":)
    NumberBody n -> ("n":) . writeString (bsShow n)
    StringBody s -> ("s":) . writeString s
    BooleanBody True -> ("t":)
    BooleanBody False -> ("f":)

defaultPriority = 0

data ValueBody = NullBody
               | NumberBody Double
               | StringBody BS.ByteString
               | BooleanBody Bool

data Value = Value { getValueSigner :: Cr.PublicKey
                   , getValuePriority :: Int
                   , getValueACL :: ACL.ACL
                   , getValueBody :: ValueBody }

valueSigner :: L.Lens' Value Cr.PublicKey
valueSigner =
  L.lens getValueSigner (\x v -> x { getValueSigner = v })

valuePriority =
  L.lens getValuePriority (\x v -> x { getValuePriority = v })

valueACL :: L.Lens' Value ACL.ACL
valueACL =
  L.lens getValueACL (\x v -> x { getValueACL = v })

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

value priority body = Value undefined priority ACL.empty body

data ValueState = ValueState { getValueStateString :: BS.ByteString
                             , getValueStateValue :: Value }

valueStateString :: L.Lens' ValueState BS.ByteString
valueStateString =
  L.lens getValueStateString (\x v -> x { getValueStateString = v })

valueStateValue :: L.Lens' ValueState Value
valueStateValue =
  L.lens getValueStateValue (\x v -> x { getValueStateValue = v })

instance Co.ParseState ValueState where
  parseString = valueStateString

parseValue signer acl s =
  getValueStateValue
  <$> (eitherToMaybe $ exec parseValue' $ ValueState s
       $ Value signer defaultPriority acl NullBody)

priority :: Co.Action ValueState ()
priority = do
  prefix "p"
  B.toInteger >>= set (valueStateValue . valuePriority)

nullBody = do
  prefix "_"
  set (valueStateValue . valueBody) NullBody

-- todo: use an efficient binary encoding
numberBody = do
  prefix "n"
  B.toInteger
  >>= stringOfSize
  >>= set (valueStateValue . valueBody) . NumberBody . bsRead

stringBody = do
  prefix "s"
  B.toInteger
  >>= stringOfSize
  >>= set (valueStateValue . valueBody) . StringBody

booleanBody =
  let put = set (valueStateValue . valueBody) . BooleanBody in
  (prefix "t" >>| prefix "f")
  >>= \case
    "t" -> put True
    "f" -> put False
    _ -> patternFailure

parseValue' :: Co.Action ValueState ()
parseValue' = do
  zeroOrOne priority
  nullBody >>| numberBody >>| stringBody >>| booleanBody
  endOfStream ()

version = 0 :: Integer
