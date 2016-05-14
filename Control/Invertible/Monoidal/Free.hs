{-# LANGUAGE GADTs, RankNTypes, TupleSections, Safe #-}
-- |
-- A vague analog of free monads for invariant monoidals.
-- This can provide a simple basis for things like invertible parsers.
module Control.Invertible.Monoidal.Free
  ( Free(..)
  , produceList
  , consumeList
  , reverseList
  ) where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.State (StateT(..))

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

-- |Given a way to extract @b@ elements from any @f a@, use a 'Free' applied to a value to produce a list of @b@ elements by converting '>*<' to '++'.
produceList :: (forall a' . f a' -> a' -> b) -> Free f a -> a -> [b]
produceList _ Empty () = []
produceList t (Transform f p) a = produceList t p $ I.biFrom f a
produceList t (Join p q) (a, b) = produceList t p a ++ produceList t q b
produceList t (Choose p _) (Left a) = produceList t p a
produceList t (Choose _ p) (Right a) = produceList t p a
produceList t (Free x) a = [t x a]

-- |Given a way to convert @b@ elements into any @f a@, use a 'Free' to parse a list of @b@ elements into a value.
-- This is the inverse of 'produceList', provided the given conversions are themselves inverses.
consumeList :: MonadPlus m => (forall a' . f a' -> b -> m a') -> Free f a -> StateT [b] m a
consumeList _ Empty = return ()
consumeList t (Transform f p) = I.biTo f <$> consumeList t p
consumeList t (Join p q) = (,) <$> consumeList t p <*> consumeList t q
consumeList t (Choose p q) = Left <$> consumeList t p <|> Right <$> consumeList t q
consumeList t (Free x) = StateT f where
  f (a:l) = (, l) <$> t x a
  f [] = mzero

-- |Flip the effective order of each '>*<' operation in a 'Free', so that 'produceList' or 'consumeList' produce or consume reversed lists.
-- This also lets you reverse the order of parsing in 'consumeList'.
-- It probably goes without saying, but applying this to an infinite structure, such as those produced by 'manyI', will not terminate.
reverseList :: Free f a -> Free f a
reverseList (Transform f (Join p q)) = Transform (f I.. I.swap) $ Join (reverseList q) (reverseList p)
reverseList (Transform f p) = Transform f $ reverseList p
reverseList (Join p q) = Transform I.swap $ Join (reverseList q) (reverseList p)
reverseList (Choose p q) = Choose (reverseList p) (reverseList q)
reverseList p = p
