{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Data.Maybe as Maybe
import Data.Either as Either
import Data.Functor as Functor
import Data.List as List
import Data.Tuple as Tuple
import Data.Foldable as Foldable
import Data.String as String
import Data.Char as Char
import GHC.Show as Show
import Data.Eq as Eq
import Data.Ord as Ord
import GHC.Enum as Enum
import GHC.Int as Int
import GHC.Num as Num
import GHC.Real as Real
import GHC.Read as Read
import Text.Read as Read
import Data.Semigroup as Semigroup
import System.IO as IO
import Data.Monoid as Monoid
import Control.Applicative as Applicative
import Data.Bifunctor as Bifunctor
import Data.Traversable as Traversable
import Control.Monad as Monad

import Data.Function as Function
import Control.DeepSeq (($!!), NFData)
import Data.Bool as Bool
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

mapEach :: (Traversable t, Monad f) => (a -> f b) -> t a -> f (t b)
mapEach = mapM

mapEach_ :: (Traversable t, Monad f) => (a -> f b) -> t a -> f ()
mapEach_ f = void . mapM f

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

forEach :: (Traversable t, Monad f) => t a -> (a -> f b) -> f (t b)
forEach = forM

forEach_ :: (Traversable t, Monad f) => t a -> (a -> f b) -> f ()
forEach_ list = void . forM list

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

is :: Eq a => a -> a -> Bool
is = (==)

safeHead :: [a] -> Maybe a
safeHead = \case
  (x:_) -> Just x
  _ -> Nothing

safeTail :: [a] -> Maybe [a]
safeTail = \case
  (_:xs) -> Just xs
  _ -> Nothing

identity :: a -> a
identity = id

length :: Num b => [a] -> b
length = genericLength

class SuperComposition a b c | a b -> c where
    (...) :: a -> b -> c

infixl 8 ...

instance {-# INCOHERENT #-} (a ~ c, r ~ b) => SuperComposition (a -> b) c r where
    f ... g = f g
    {-# INLINE (...) #-}

instance {-# INCOHERENT #-} (SuperComposition (a -> b) d r1, r ~ (c -> r1)) => SuperComposition (a -> b) (c -> d) r where
    (f ... g) c = f ... g c
    {-# INLINE (...) #-}
