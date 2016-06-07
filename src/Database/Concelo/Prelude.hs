module Database.Concelo.Prelude
  ( null
  , foldM
  , forM_
  , mapM_
  , length
  , (++)
  , (+)
  , (-)
  , (*)
  , (/)
  , ($)
  , (>)
  , (<)
  , (>=)
  , (<=)
  , (/=)
  , (&&)
  , (||)
  , (!!)
  , (.)
  , not
  , error
  , undefined
  , otherwise
  , flip
  , fromIntegral
  , toEnum
  , fst
  , snd
  , id
  , const
  , reverse
  , concat
  , take
  , repeat
  , last
  , filter
  , floor
  , zipWith
  , Double()
  , Bool(True, False)
  , String
  , Int()
  , Integer()
  , IO()
  , Show(show)
  , Read
  , read
  , Eq((==))
  , Ord(compare)
  , Ordering(EQ)
  , Maybe(Just, Nothing)
  , fromMaybe
  , isJust
  , fromJust
  , isNothing
  , maybe
  , Either(Left, Right)
  , Monad
  , (>>=)
  , (>>)
  , return
  , liftM2
  , liftM3
  , when
  , mapM
  , Foldable(foldr)
  , toList
  , foldl'
  , Functor(fmap)
  , Traversable(traverse)
  , pure
  , (<$>)
  , (<*>)
  , (<|>)
  , inits ) where

import Prelude (($), (.), error, undefined, String, Bool(True, False),
                flip, Int(), (+), (-), Show(show), Read, read, otherwise,
                fromIntegral, (*), (/), (>), (<), (>=), (<=), (/=), toEnum,
                fst, snd, id, (&&), (||), not, Eq((==)), Ord(compare),
                Integer(), const, (++), reverse, concat, Double(), (!!),
                take, repeat, last, filter, Ordering(EQ), IO(), zipWith,
                floor)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, fromJust,
                   isNothing, maybe)
import Data.Either (Either(Left, Right))
import Data.Foldable (Foldable(foldr), toList, foldl')
import Control.Monad (Monad, (>>=), (>>), return, liftM2, liftM3, when, mapM)
import Data.Functor (Functor(fmap), (<$>))
import Control.Applicative ((<|>), (<*>), pure)
import Data.Traversable (Traversable(traverse))
import Data.List (inits)

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
