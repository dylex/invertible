-- |
-- Invariant monoidal functors.
{-# LANGUAGE Safe, FlexibleInstances #-}
module Control.Invariant.Monoidal
  ( -- * Functor
    (>$<)
  -- * Monoidal
  , Monoidal(..)
  , (>*), (*<)
  -- ** Tuple combinators
  , liftI2
  , liftI3
  , liftI4
  , liftI5
  , (>*<<)
  , (>*<<<)
  , (>*<<<<)
  , (>>*<)
  , (>>>*<)
  , (>>>>*<)
  , (>>*<<)
  -- * MonoidalPlus
  , MonoidalPlus(..)
  , possible
  ) where

import Prelude hiding (Functor(..), (<$>), fst, snd, id)
import Control.Arrow ((&&&), (***))

import Data.Isomorphism.Type
import Data.Isomorphism.Prelude (id)
import Data.Isomorphism.Either (lft)
import Data.Isomorphism.Tuple
import Control.Invariant.Functor

-- |Another synonym for 'fmap' to match other operators in this module.
(>$<) :: Functor f => a <-> b -> f a -> f b
(>$<) = fmap

infixl 4 >$<

-- |Lax invariant monoidal functor.
-- This roughly corresponds to 'Applicative', which, for covariant functors, is equivalent to a monoidal functor.
-- Invariant functors, however, may admit a monoidal instance but not applicative.
class Functor f => Monoidal f where
  -- |Lift a unit value, analogous to @'Control.Applicative.pure' ()@ (but also like @const ()@).
  unit :: f ()
  -- |Merge two functors into a tuple, analogous to @'Control.Applicative.liftA2' (,)@. (Sometimes known as @**@.)
  (>*<) :: f a -> f b -> f (a, b)

-- | Sequence actions, discarding/inhabiting the unit value of the first argument.
(*<) :: Monoidal f => f () -> f a -> f a
(*<) = liftI2 snd

-- | Sequence actions, discarding/inhabiting the unit value of the second argument.
(>*) :: Monoidal f => f a -> f () -> f a
(>*) = liftI2 fst

infixl 4 >*, >*<, *<

-- |Lift an (uncurried) isomorphism into a monoidal functor.
liftI2 :: Monoidal f => ((a, b) <-> c) -> f a -> f b -> f c
liftI2 f a b = f <$> (a >*< b)

liftI3 :: Monoidal f => ((a, b, c) <-> d) -> f a -> f b -> f c -> f d
liftI3 f a b c = f <$> (a >*< b >>*< c)

liftI4 :: Monoidal f => ((a, b, c, d) <-> e) -> f a -> f b -> f c -> f d -> f e
liftI4 f a b c d = f <$> (a >*< b >>*< c >>>*< d)

liftI5 :: Monoidal f => ((a, b, c, d, e) <-> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftI5 f a b c d e = f <$> (a >*< b >>*< c >>>*< d >>>>*< e)

(>>*<) :: Monoidal f => f (a, b) -> f c -> f (a, b, c)
(>>*<) = liftI2 flatten2_1

(>>>*<) :: Monoidal f => f (a, b, c) -> f d -> f (a, b, c, d)
(>>>*<) = liftI2 flatten3_1

(>>>>*<) :: Monoidal f => f (a, b, c, d) -> f e -> f (a, b, c, d, e)
(>>>>*<) = liftI2 flatten4_1

infixl 4 >>*<, >>>*<, >>>>*<

(>*<<) :: Monoidal f => f a -> f (b, c) -> f (a, b, c)
(>*<<) = liftI2 flatten1_2

(>*<<<) :: Monoidal f => f a -> f (b, c, d) -> f (a, b, c, d)
(>*<<<) = liftI2 flatten1_3

(>*<<<<) :: Monoidal f => f a -> f (b, c, d, e) -> f (a, b, c, d, e)
(>*<<<<) = liftI2 flatten1_4

infixr 3 >*<<, >*<<<, >*<<<<

(>>*<<) :: Monoidal f => f (a, b) -> f (c, d) -> f (a, b, c, d)
(>>*<<) = liftI2 flatten2_2

infix 3 >>*<<

instance Monoidal (Isomorphism (->) ()) where
  unit = id
  -- |Uses the 'Monoid' instance to combine '()'s.
  (ua :<->: au) >*< (ub :<->: bu) = ua &&& ub :<->: uncurry mappend . (au *** bu)

-- |Monoidal functors that allow choice.
class Monoidal f => MonoidalPlus f where
  -- |An always-failing (and ignoring) value.
  zero :: f a
  -- |Associative binary choice.
  (>|<) :: f a -> f b -> f (Either a b)

infixl 3 >|<

-- |Analogous to 'Control.Applicative.optional'.
possible :: MonoidalPlus f => f a -> f (Maybe a)
possible f = lft <$> (f >|< unit)
