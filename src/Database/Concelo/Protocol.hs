module Database.Concelo.Protocol
  ( Message(Cred) ) where

data Message = Cred { getCredRequest :: Int
                    , getCredPublic :: ByteString }
