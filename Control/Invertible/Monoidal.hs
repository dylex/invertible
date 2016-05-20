-- |
-- Invariant monoidal functors.
-- 
-- This roughly corresponds to "Control.Applicative", but exposes a non-overlapping API so can be imported unqualified.  It does, however, use operators similar to those provided by contravariant.
{-# LANGUAGE CPP, Safe, TypeOperators, FlexibleInstances #-}
module Control.Invertible.Monoidal
  ( -- * Functor
    (>$<)
  , (>$), ($<)
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
  , pureI
  , sequenceI_
  , mapI_
  , forI_
  , sequenceMaybesI
  , mapMaybeI
  -- * MonoidalAlt
  , MonoidalAlt(..)
  , (>|), (|<)
  , optionalI
  , defaulting
  , manyI
  ) where

import Prelude
import Control.Arrow ((&&&), (***))

import Data.Invertible.Bijection
import qualified Data.Invertible as I

-- |Another synonym for 'fmap' to match other operators in this module.
(>$<) :: I.Functor f => a <-> b -> f a -> f b
(>$<) = I.fmap

infixl 4 $<, >$<, >$

-- |Given a value an an invariant for that value, always provide that value and ignore the produced value.
-- @'I.fmap' . flip 'I.consts' ()@
(>$) :: I.Functor f => a -> f a -> f ()
(>$) a = I.fmap $ I.consts a ()

-- |@flip ('>$')@
($<) :: I.Functor f => f a -> a -> f ()
($<) = flip (>$)

-- |Invariant monoidal functor.
-- This roughly corresponds to 'Applicative', which, for covariant functors, is equivalent to a monoidal functor.
-- Invariant functors, however, may admit a monoidal instance but not applicative.
class I.Functor f => Monoidal f where
  -- |Lift a unit value, analogous to @'Control.Applicative.pure' ()@ (but also like @const ()@).
  unit :: f ()
  -- |Merge two functors into a tuple, analogous to @'Control.Applicative.liftA2' (,)@. (Sometimes known as @**@.)
  (>*<) :: f a -> f b -> f (a, b)

-- |Sequence actions, discarding/inhabiting the unit value of the second argument.
(>*) :: Monoidal f => f a -> f () -> f a
(>*) = liftI2 I.fst

-- |Sequence actions, discarding/inhabiting the unit value of the first argument.
(*<) :: Monoidal f => f () -> f a -> f a
(*<) = liftI2 I.snd

infixl 4 >*, >*<, *<

-- |Lift an (uncurried) bijection into a monoidal functor.
liftI2 :: Monoidal f => ((a, b) <-> c) -> f a -> f b -> f c
liftI2 f a b = f >$< (a >*< b)

liftI3 :: Monoidal f => ((a, b, c) <-> d) -> f a -> f b -> f c -> f d
liftI3 f a b c = f >$< (a >*< b >>*< c)

liftI4 :: Monoidal f => ((a, b, c, d) <-> e) -> f a -> f b -> f c -> f d -> f e
liftI4 f a b c d = f >$< (a >*< b >>*< c >>>*< d)

liftI5 :: Monoidal f => ((a, b, c, d, e) <-> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftI5 f a b c d e = f >$< (a >*< b >>*< c >>>*< d >>>>*< e)

(>>*<) :: Monoidal f => f (a, b) -> f c -> f (a, b, c)
(>>*<) = liftI2 I.flatten2_1

(>>>*<) :: Monoidal f => f (a, b, c) -> f d -> f (a, b, c, d)
(>>>*<) = liftI2 I.flatten3_1

(>>>>*<) :: Monoidal f => f (a, b, c, d) -> f e -> f (a, b, c, d, e)
(>>>>*<) = liftI2 I.flatten4_1

infixl 4 >>*<, >>>*<, >>>>*<

(>*<<) :: Monoidal f => f a -> f (b, c) -> f (a, b, c)
(>*<<) = liftI2 I.flatten1_2

(>*<<<) :: Monoidal f => f a -> f (b, c, d) -> f (a, b, c, d)
(>*<<<) = liftI2 I.flatten1_3

(>*<<<<) :: Monoidal f => f a -> f (b, c, d, e) -> f (a, b, c, d, e)
(>*<<<<) = liftI2 I.flatten1_4

infixr 3 >*<<, >*<<<, >*<<<<

(>>*<<) :: Monoidal f => f (a, b) -> f (c, d) -> f (a, b, c, d)
(>>*<<) = liftI2 I.flatten2_2

infix 3 >>*<<

-- |A constant monoidal (like 'Control.Applicative.pure'), which always produces the same value and ignores everything.
pureI :: Monoidal f => a -> f a
pureI a = I.const a >$< unit

-- |Sequence (like 'Data.Foldable.sequenceA_') a list of monoidals, ignoring (@'I.const' ()@) all the results.
sequenceI_ :: (Foldable t, Monoidal f) => t (f ()) -> f ()
sequenceI_ = foldr (*<) unit

-- |Map each element to a monoidal and 'sequenceI_' the results.
mapI_ :: (Foldable t, Monoidal f) => (a -> f ()) -> t a -> f ()
mapI_ f = foldr ((*<) . f) unit

-- |@flip 'mapI_'@
forI_ :: (Foldable t, Monoidal f) => t a -> (a -> f ()) -> f ()
forI_ = flip mapI_

-- |Sequence (like 'Data.Traversable.sequenceA') and filter (like 'Data.Maybe.catMaybes') a list of monoidals, producing the list of non-'Nothing' values.
-- Shorter input lists pad with 'Nothing's and longer ones are ignored.
sequenceMaybesI :: Monoidal f => [f (Maybe a)] -> f [a]
sequenceMaybesI [] = pureI []
sequenceMaybesI (x:l) = liftI2 I.consMaybe x (sequenceMaybesI l)

-- |Map each element to a 'Maybe' monoidal and sequence the results (like 'Data.Traversable.traverse' and 'Data.Maybe.mapMaybe').
mapMaybeI :: Monoidal f => (a -> f (Maybe b)) -> [a] -> f [b]
mapMaybeI = (sequenceMaybesI .) . map

-- |Monoidal functors that allow choice.
class Monoidal f => MonoidalAlt f where
  -- |Associative binary choice.
  (>|<) :: f a -> f b -> f (Either a b)

-- |Assymetric version of '>|<' that returns whichever action succeeds but always uses the left one on inputs.
(>|) :: MonoidalAlt f => f a -> f a -> f a
a >| b = (either id id :<->: Left) >$< (a >|< b)

-- |Assymetric version of '>|<' that returns whichever action succeeds but always uses the right one on inputs.
(|<) :: MonoidalAlt f => f a -> f a -> f a
a |< b = (either id id :<->: Right) >$< (a >|< b)

infixl 3 >|, >|<, |<

-- |Analogous to 'Control.Applicative.optional'.
optionalI :: MonoidalAlt f => f a -> f (Maybe a)
optionalI f = I.lft >$< (f >|< unit)

-- |Return a default value if a monoidal functor fails, and only apply it to non-default values.
defaulting :: (MonoidalAlt f, Eq a) => a -> f a -> f a
defaulting a f = I.fromMaybe a >$< optionalI f

-- |Repeatedly apply a monoidal functor until it fails.  Analogous to 'Control.Applicative.many'.
manyI :: MonoidalAlt f => f a -> f [a]
manyI f = I.cons >$< optionalI (f >*< manyI f)


instance Monoidal (Bijection (->) ()) where
  unit = I.id
  -- |Uses the 'Monoid' instance to combine '()'s.
  (ua :<->: au) >*< (ub :<->: bu) = ua &&& ub :<->: uncurry mappend . (au *** bu)
