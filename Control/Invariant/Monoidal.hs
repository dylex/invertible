-- |
-- Invariant monoidal functors.
{-# LANGUAGE Safe, FlexibleInstances #-}
module Control.Invariant.Monoidal
  ( Monoidal(..)
  , (>*), (*<)
  , liftI2
  , MonoidalPlus(..)
  , possible
  ) where

import Prelude hiding (Functor(..), (<$>), fst, snd, id)
import Control.Arrow ((&&&), (***))

import Data.Isomorphism.Type
import Data.Isomorphism.Prelude (fst, snd, id)
import Data.Isomorphism.Either (lft)
import Control.Invariant.Functor

-- |Lax invariant monoidal functor.
-- This roughly corresponds to 'Applicative', which, for covariant functors, is equivalent to a monoidal functor.
-- Invariant functors, however, may admit a monoidal instance but not applicative.
class Functor f => Monoidal f where
  -- |Lift a unit value, analogous to @'Control.Applicative.pure' ()@ (but also like @const ()@).
  unit :: f ()
  -- |Merge two functors into a tuple, analogous to @'Control.Applicative.liftA2' (,)@. (Sometimes known as @**@.)
  (>*<) :: f a -> f b -> f (a, b)

-- | Sequence actions, discarding/inhabiting the unit value of the first argument.
(>*) :: Monoidal f => f () -> f a -> f a
(>*) = liftI2 snd

-- | Sequence actions, discarding/inhabiting the unit value of the second argument.
(*<) :: Monoidal f => f a -> f () -> f a
(*<) = liftI2 fst

infixl 4 >*, >*<, *<

-- |Lift an (uncurried) isomorphism into a monoidal functor.
liftI2 :: Monoidal f => ((a, b) <-> c) -> f a -> f b -> f c
liftI2 f a b = f <$> (a >*< b)

instance Monoidal (Isomorphism (->) ()) where
  unit = id
  -- |Uses the 'Monoid' instance to combine '()'s.
  (ua :<->: au) >*< (ub :<->: bu) = ua &&& ub :<->: uncurry mappend . (au *** bu)

-- |Monoidal functors that allow choice.
class Monoidal f => MonoidalPlus f where
  -- |An always-failing value.
  zero :: f a
  -- |Associative binary choice.
  (>|<) :: f a -> f b -> f (Either a b)

infixl 3 >|<

-- |Analogous to 'Control.Applicative.optional'.
possible :: MonoidalPlus f => f a -> f (Maybe a)
possible f = lft <$> (f >|< unit)
