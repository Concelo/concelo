module Database.Concelo.Revision
  ( Revision
  , Values
  , revisionTrie
  , revisionUpdateTime
  , revisionNumber ) where

import Database.Concelo.Trie (Trie)

import qualified Control.Lens as L

data Value = Value { getValueACL :: ACL }

type ValueTrie = Trie ByteString Value

data Revision = Revision { getRevisionTrie :: ValueTrie
                         , getRevisionUpdateTime :: Integer
                         , getRevisionNumber :: Integer }

revisionTrie = L.lens getRevisionTrie (\x v -> x { getRevisionTrie = v })

revisionUpdateTime =
  L.lens getRevisionUpdateTime (\x v -> x { getRevisionUpdateTime = v })

revisionNumber = L.lens getRevisionNumber (\x v -> x { getRevisionNumber = v })
