-- |
-- A symmetric version of the Kleisli monad transformer arrow.
-- BiKleisli provides this Kleisli-like arrow over bijections.
--
-- The Alimarine paper just calls it \"MoT\" for Monad Transformer.
{-# LANGUAGE CPP, Safe, TupleSections, FlexibleInstances, FlexibleContexts #-}
module Control.Invertible.MonadArrow
  ( MonadArrow(..)
  , BiKleisli
  ) where

import Prelude hiding (id, (.))

import Control.Category
import Control.Arrow
import Control.Monad (MonadPlus(..))
#ifdef VERSION_semigroupoids
import Data.Semigroupoid (Semigroupoid(..))
import Data.Groupoid (Groupoid(..))
#endif

import Data.Invertible.Bijection
import Control.Invertible.BiArrow

-- |Bidirectional 'Control.Arrow.Kleisli'-like monad arrow transformer.
newtype MonadArrow a m b c = MonadArrow { runMonadArrow :: a (m b) (m c) }

-- |A MonadArrow over bijections.
type BiKleisli m a b = MonadArrow (<->) m a b

instance Category a => Category (MonadArrow a m) where
  id = MonadArrow id
  MonadArrow f . MonadArrow g = MonadArrow (f . g)

instance Monad m => Arrow (MonadArrow (->) m) where
  arr = MonadArrow . arr . fmap
  first  (MonadArrow f) = MonadArrow (>>= \ ~(a,c) -> ( ,c) <$> f (return a))
  second (MonadArrow f) = MonadArrow (>>= \ ~(a,b) -> (a, ) <$> f (return b))
  MonadArrow f *** MonadArrow g = MonadArrow
    (>>= \ ~(a,b) -> (,) <$> f (return a) <*> g (return b))
  MonadArrow f &&& MonadArrow g = MonadArrow
    (>>= \ a -> let ma = return a in (,) <$> f ma <*> g ma)

instance Monad m => ArrowChoice (MonadArrow (->) m) where
  left  (MonadArrow f) = MonadArrow (>>= either (fmap Left . f . return) (return . Right))
  right (MonadArrow f) = MonadArrow (>>= either (return . Left) (fmap Right . f . return))
  MonadArrow f +++ MonadArrow g = MonadArrow
    (>>= either (fmap Left . f . return) (fmap Right . g . return))
  MonadArrow f ||| MonadArrow g = MonadArrow
    (>>= either (f . return) (g . return))

instance MonadPlus m => ArrowZero (MonadArrow (->) m) where
  zeroArrow = MonadArrow (const mzero)

instance MonadPlus m => ArrowPlus (MonadArrow (->) m) where
  MonadArrow f <+> MonadArrow g = MonadArrow (>>= \x -> let mx = return x in f mx `mplus` g mx)

liftMoA :: (MonadArrow (->) m a b -> MonadArrow (->) m c d) -> (m a -> m b) -> (m c -> m d)
liftMoA t = runMonadArrow . t . MonadArrow

liftMoA2 :: (MonadArrow (->) m a b -> MonadArrow (->) m c d -> MonadArrow (->) m e f) -> (m a -> m b) -> (m c -> m d) -> (m e -> m f)
liftMoA2 t f g = runMonadArrow (MonadArrow f `t` MonadArrow g)

instance Monad m => Arrow (MonadArrow (<->) m) where
  arr = MonadArrow . arr . fmap
  first (MonadArrow (f :<->: g)) = MonadArrow $ bik f :<->: bik g
    where bik = liftMoA first
  second (MonadArrow (f :<->: g)) = MonadArrow $ bik f :<->: bik g
    where bik = liftMoA second
  MonadArrow (f :<->: g) *** MonadArrow (f' :<->: g') =
    MonadArrow $ bik f f' :<->: bik g g'
    where bik = liftMoA2 (***)
  MonadArrow (f :<->: g) &&& MonadArrow (f' :<->: _) =
    MonadArrow $ liftMoA2 (&&&) f f' :<->: (g . arr (fmap fst)) -- (g' . arr snd)

instance Monad m => ArrowChoice (MonadArrow (<->) m) where
  left  (MonadArrow (f :<->: g)) = MonadArrow $ bik f :<->: bik g
    where bik = liftMoA left
  right (MonadArrow (f :<->: g)) = MonadArrow $ bik f :<->: bik g
    where bik = liftMoA right
  MonadArrow (f :<->: g) +++ MonadArrow (f' :<->: g') =
    MonadArrow $ bik f f' :<->: bik g g'
    where bik = liftMoA2 (+++)
  MonadArrow (f :<->: g) ||| MonadArrow (f' :<->: _) =
    MonadArrow $ liftMoA2 (|||) f f' :<->: (arr (fmap Left) . g) -- (arr (fmap Right) . g)

instance MonadPlus m => ArrowZero (MonadArrow (<->) m) where
  zeroArrow = MonadArrow (const mzero :<->: const mzero)

instance MonadPlus m => ArrowPlus (MonadArrow (<->) m) where
  MonadArrow (f1 :<->: g1) <+> MonadArrow (f2 :<->: g2) =
    MonadArrow $ bik f1 f2 :<->: bik g1 g2
    where bik = liftMoA2 (<+>)

instance (BiArrow a, Monad m) => BiArrow (MonadArrow a m) where
  f <-> g = MonadArrow (arr (fmap f) <-> arr (fmap g))
  invert (MonadArrow f) = MonadArrow (invert f)

instance Monad m => BiArrow' (MonadArrow (<->) m)

#ifdef VERSION_semigroupoids
instance Semigroupoid a => Semigroupoid (MonadArrow a m) where
  MonadArrow f `o` MonadArrow g = MonadArrow (f `o` g)

instance Groupoid a => Groupoid (MonadArrow a m) where
  inv (MonadArrow f) = MonadArrow (inv f)
#endif
