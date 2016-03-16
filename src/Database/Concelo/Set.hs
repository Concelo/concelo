module Database.Concelo.Set
  ( Set()
  , empty
  , value
  , insert
  , delete ) where

import qualified Database.Concelo.Map as M

newtype Set v = Set { run :: M.Map v () }

empty = Set M.empty

value = M.key . run

insert v = Set . M.insert v () . run

delete v = Set . M.delete v . run
