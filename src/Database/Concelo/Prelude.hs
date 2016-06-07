module Database.Concelo.Prelude
  ( (!!)
  , ($)
  , (&&)
  , (*)
  , (+)
  , (++)
  , (-)
  , (.)
  , (/)
  , (/=)
  , (<$>)
  , (<)
  , (<*>)
  , (<=)
  , (<|>)
  , (>)
  , (>=)
  , (>>)
  , (>>=)
  , (||)
  , Bool(True, False)
  , Double()
  , Either(Left, Right)
  , Eq((==))
  , Foldable(foldr)
  , Functor(fmap)
  , IO()
  , Int()
  , Integer()
  , Maybe(Just, Nothing)
  , Monad
  , Ord(compare)
  , Ordering(EQ)
  , Read
  , Show(show)
  , String
  , Traversable(traverse)
  , concat
  , const
  , error
  , filter
  , flip
  , floor
  , foldM
  , foldl'
  , forM_
  , fromIntegral
  , fromJust
  , fromMaybe
  , fst
  , id
  , init
  , inits
  , isJust
  , isNothing
  , last
  , length
  , liftM2
  , liftM3
  , mapM
  , mapM_
  , maybe
  , not
  , null
  , otherwise
  , pure
  , read
  , repeat
  , return
  , reverse
  , snd
  , sum
  , tail
  , take
  , toEnum
  , toList
  , undefined
  , when
  , zipWith
  ) where

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
import Data.Foldable (Foldable(foldr), toList, foldl', sum)
import Control.Monad (Monad, (>>=), (>>), return, liftM2, liftM3, when, mapM)
import Data.Functor (Functor(fmap), (<$>))
import Control.Applicative ((<|>), (<*>), pure)
import Data.Traversable (Traversable(traverse))
import Data.List (inits, init, tail)

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
