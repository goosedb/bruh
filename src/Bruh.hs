{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
module Bruh 
  ( module Bruh
  , Maybe.fromMaybe
  , Maybe.maybe
  , Maybe.Maybe(..)

  , Either.either
  , Either.Either(..)

  , Functor.void
  , (Functor.$>)
  , (Functor.<$>)
  , Functor.Functor(..)

  , Bifunctor.Bifunctor(..)

  , Applicative.Applicative(pure, (<*>), (<*), (*>))
  , Applicative.Alternative(empty, (<|>))
  
  , Monad.Monad ((>>=), (>>))
  , (Monad.>=>)

  , Bool.Bool (..)

  , Foldable.Foldable
  , Foldable.null
  , Foldable.fold
  , Foldable.foldl
  , Foldable.foldl'
  , Foldable.foldr
  , Foldable.foldr'
  , Foldable.elem
  , Foldable.and
  , Foldable.or
  , Foldable.any
  , Foldable.foldMap
  , Foldable.all

  , Traversable.Traversable (..)

  , List.concat
  , List.take
  , List.takeWhile
  , List.dropWhile
  , List.drop
  , List.filter
  , List.zip
  , List.zipWith

  , Tuple.curry
  , Tuple.uncurry
  , Tuple.fst
  , Tuple.snd


  , (Function..)
  
  , Show.Show(show)

  , Eq.Eq(..)

  , Ord.Ord(..)

  , String.String
  , String.lines
  , String.unlines
  , String.words
  , String.unwords

  , Char.Char
  , Char.isLower
  , Char.isUpper
  , Char.isSpace

  , Enum.Enum

  , Read.Read 
  , Read.readMaybe
  , Read.read

  , Int
  , Integer
  , Float
  , Double
  , Rational

  , Num.Num((+), (-), (*))
  , Real.Real(..)
  , Integral (mod, div)
  , Fractional ((/), fromRational)
  , Floating
  , RealFrac(round, floor, ceiling)

  , Semigroup.Semigroup(..)
  , Monoid.Monoid(..)

  , IO.putStrLn
  , IO.putStr
  , IO.print
  , IO.getLine
  , IO.getChar 
  , IO.IO

  ) where

import GHC.Stack (HasCallStack, CallStack, callStack)
import Control.Exception (Exception, throw)
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.Functor as Functor
import qualified Data.List as List
import qualified Data.Tuple as Tuple
import qualified Data.Foldable as Foldable
import qualified Data.String as String
import qualified Data.Char as Char
import qualified GHC.Show as Show
import qualified Data.Eq as Eq
import qualified Data.Ord as Ord
import qualified GHC.Enum as Enum
import qualified GHC.Int as Int
import qualified GHC.Num as Num
import qualified GHC.Real as Real
import qualified GHC.Read as Read 
import qualified Text.Read as Read
import qualified Data.Semigroup as Semigroup
import qualified System.IO as IO
import qualified Data.Monoid as Monoid
import qualified Control.Applicative as Applicative
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Traversable as Traversable
import qualified Control.Monad as Monad

import qualified Data.Function as Function
import Control.DeepSeq (($!!), NFData)
import qualified Data.Bool as Bool
import GHC.OldList (genericLength)

newtype Bruh = Bruh CallStack
  deriving stock Show
  deriving anyclass Exception

bruh :: HasCallStack => a
bruh = throw (Bruh ?callStack)

(|>) :: a -> (a -> b) -> b
(|>) = (Function.&)

(|>!) :: a -> (a -> c) -> c
(|>!) = flip ($!)

(|>!!) :: NFData a => a -> (a -> b) -> b
(|>!!) = flip ($!!)

infixl 1 |>

(<|) :: (a -> b) -> a -> b
(<|) = ($)

(<|!) :: (a -> b) -> a -> b
(<|!) = ($!)

(<|!!) :: NFData a => (a -> b) -> a -> b
(<|!!) = ($!!)

infixr 0 <|, <|!, <|!!

(<||>) :: Monad m => m Bool -> m Bool -> m Bool
a <||> b = a >>= Bool.bool b (pure True)

(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
a <&&> b = a >>= Bool.bool (pure False) b

while :: Monad m => m Bool -> m ()
while action = action >>= Bool.bool (pure ()) (while action)

forEach :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
forEach = traverse

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap 

mapEach :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
mapEach list f = traverse f list

map :: Functor f => (a -> b) -> f a -> f b
map = fmap 

is :: Eq a => a -> a -> Bool
is = (==)

maybeHead :: [a] -> Maybe a
maybeHead = \case
  (x:_) -> Just x
  _ -> Nothing

identity :: a -> a
identity = id 

length :: Num b => [a] -> b
length = genericLength