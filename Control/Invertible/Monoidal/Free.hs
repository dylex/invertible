{-# LANGUAGE GADTs, RankNTypes, TupleSections, Safe #-}
-- |
-- A vague analog of free monads for invariant monoidals.
-- This can provide a simple basis for things like invertible parsers.
module Control.Invertible.Monoidal.Free
  ( Free(..)
  , foldFree
  , produceFree
  , runFree
  , parseFree
  , reverseFree
  ) where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.State (StateT(..))
import Data.Monoid ((<>), Alt(..))

import Control.Invertible.Monoidal
import qualified Data.Invertible as I

-- |Produce a 'MonoidalAlt' out of any type constructor, simply by converting each monoidal operation into a constructor.
-- Although a version more analogous to a free monad could be defined for instances of 'I.Functor' and restricted to 'Monoidal', including the Yoneda transform makes this the more general case.
data Free f a where
  Empty :: Free f ()
  Transform :: !(a I.<-> b) -> Free f a -> Free f b
  Join :: Free f a -> Free f b -> Free f (a, b)
  Choose :: Free f a -> Free f b -> Free f (Either a b)
  Free :: !(f a) -> Free f a

instance I.Functor (Free f) where
  fmap f (Transform g p) = Transform (f I.. g) p
  fmap f p = Transform f p

instance Monoidal (Free f) where
  unit = Empty
  (>*<) = Join

instance MonoidalAlt (Free f) where
  (>|<) = Choose

-- |Transform the type constructor within a 'Free'.
mapFree :: (forall a' . f a' -> m a') -> Free f a -> Free m a
mapFree _ Empty = Empty
mapFree t (Transform f p) = Transform f $ mapFree t p
mapFree t (Join p q) = Join (mapFree t p) (mapFree t q)
mapFree t (Choose p q) = Choose (mapFree t p) (mapFree t q)
mapFree t (Free x) = Free (t x)

-- |Given a way to extract a @b@ from any @f a@, use a 'Free' applied to a value to produce a @b@ by converting '>*<' to '<>'.
foldFree :: Monoid b => (forall a' . f a' -> a' -> b) -> Free f a -> a -> b
foldFree _ Empty () = mempty
foldFree t (Transform f p) a = foldFree t p $ I.biFrom f a
foldFree t (Join p q) (a, b) = foldFree t p a <> foldFree t q b
foldFree t (Choose p _) (Left a) = foldFree t p a
foldFree t (Choose _ p) (Right a) = foldFree t p a
foldFree t (Free x) a = t x a

-- |'foldFree' over Alternative rather than Monoid.
produceFree :: Alternative m => (forall a' . f a' -> a' -> b) -> Free f a -> a -> m b
produceFree t f = getAlt . foldFree (\x a -> Alt $ pure $ t x a) f

-- |Evaluate a 'Free' into an underlying 'MonadPlus', by evaluating '>|<' with '<|>'.
runFree :: MonadPlus f => Free f a -> f a
runFree Empty = return ()
runFree (Transform f p) = I.biTo f <$> runFree p
runFree (Join p q) = (,) <$> runFree p <*> runFree q
runFree (Choose p q) = Left <$> runFree p <|> Right <$> runFree q
runFree (Free x) = x

-- |Given a way to convert @b@ elements into any @f a@, use a 'Free' to parse a list of @b@ elements into a value.
-- This just uses 'I.uncons' with 'runFree', and is the inverse of 'produceFree', provided the given conversions are themselves inverses.
parseFree :: MonadPlus m => (forall a' . f a' -> b -> m a') -> Free f a -> [b] -> m (a, [b])
parseFree t = runStateT . runFree . mapFree (\x -> StateT $ \l -> case l of { (a:r) -> (, r) <$> t x a ; [] -> mzero })

-- |Flip the effective order of each '>*<' operation in a 'Free', so that processing is done in the reverse order.
-- It probably goes without saying, but applying this to an infinite structure, such as those produced by 'manyI', will not terminate.
reverseFree :: Free f a -> Free f a
reverseFree (Transform f (Join p q)) = Transform (f I.. I.swap) $ Join (reverseFree q) (reverseFree p)
reverseFree (Transform f p) = Transform f $ reverseFree p
reverseFree (Join p q) = Transform I.swap $ Join (reverseFree q) (reverseFree p)
reverseFree (Choose p q) = Choose (reverseFree p) (reverseFree q)
reverseFree p = p
