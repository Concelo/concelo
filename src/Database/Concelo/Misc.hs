module Database.Concelo.Misc
  ( null
  , foldM
  , forM_
  , mapM_
  , length ) where

import Prelude hiding (foldr, null, mapM_, length)
import Data.Foldable (Foldable(foldr), toList, foldl')

import qualified Control.Monad as M

null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM visit seed foldable = M.foldM visit seed $ toList foldable

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = M.forM_ . toList

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ = flip forM_

length :: Foldable t => t a -> Int
length = foldl' (\c _ -> c + 1) 0
