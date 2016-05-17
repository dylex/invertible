{-# LANGUAGE GADTs, RankNTypes, TupleSections, Safe #-}
-- |
-- A vague analog of free monads for invariant monoidals.
-- This can provide a simple basis for things like invertible parsers.
module Control.Invertible.Monoidal.Free
  ( Free(..)
  , mapFree
  , foldFree
  , produceFree
  , runFree
  , parseFree
  , reverseFree
  , freeTNF
  , freeTDNF
  , freeLinearTDNF
  ) where

import Control.Applicative (Alternative(..))
import Control.Arrow ((***), first, second, (+++), left, right)
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..))
import Data.Monoid ((<>), Alt(..))

import Control.Invertible.Monoidal
import qualified Data.Invertible as I

-- |Produce a 'MonoidalAlt' out of any type constructor, simply by converting each monoidal operation into a constructor.
-- Although a version more analogous to a free monad could be defined for instances of 'I.Functor' and restricted to 'Monoidal', including the Yoneda transform makes this the more general case.
data Free f a where
  Empty :: Free f ()
  Free :: !(f a) -> Free f a
  Join :: Free f a -> Free f b -> Free f (a, b)
  Choose :: Free f a -> Free f b -> Free f (Either a b)
  Transform :: !(a I.<-> b) -> Free f a -> Free f b

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

-- |Evaluate a 'Free' into an underlying 'Alternative', by evaluating '>|<' with '<|>'.
runFree :: Alternative f => Free f a -> f a
runFree Empty = pure ()
runFree (Transform f p) = I.biTo f <$> runFree p
runFree (Join p q) = (,) <$> runFree p <*> runFree q
runFree (Choose p q) = Left <$> runFree p <|> Right <$> runFree q
runFree (Free x) = x

-- |Uncons the current state, returning the head and keeping the tail, or fail if empty.
-- (Parsec's 'Text.Parsec.Prim.Stream' class provides similar but more general functionality.)
unconsState :: Alternative m => StateT [a] m a
unconsState = StateT ucs where
  ucs (a:l) = pure (a, l)
  ucs [] = empty

-- |Given a way to convert @b@ elements into any @f a@, use a 'Free' to parse a list of @b@ elements into a value.
-- This just uses 'unconsState' with 'runFree', and is the inverse of 'produceFree', provided the given conversions are themselves inverses.
parseFree :: MonadPlus m => (forall a' . f a' -> b -> m a') -> Free f a -> [b] -> m (a, [b])
parseFree t = runStateT . runFree . mapFree (\x -> lift . t x =<< unconsState)

-- |Flip the effective order of each '>*<' operation in a 'Free', so that processing is done in the reverse order.
-- It probably goes without saying, but applying this to an infinite structure, such as those produced by 'manyI', will not terminate.
reverseFree :: Free f a -> Free f a
reverseFree (Transform f (Join p q)) = Transform (f I.. I.swap) $ Join (reverseFree q) (reverseFree p)
reverseFree (Transform f p) = Transform f $ reverseFree p
reverseFree (Join p q) = Transform I.swap $ Join (reverseFree q) (reverseFree p)
reverseFree (Choose p q) = Choose (reverseFree p) (reverseFree q)
reverseFree p = p

chooseTNF :: Free f a -> Free f b -> Free f (Either a b)
chooseTNF (Transform f p) (Transform g q) = (f +++ g) >$< chooseTNF p q
chooseTNF (Transform f p) q = left f >$< chooseTNF p q
chooseTNF p (Transform g q) = right g >$< chooseTNF p q
chooseTNF p q = Choose p q

joinTNF :: Free f a -> Free f b -> Free f (a, b)
joinTNF (Transform f p) (Transform g q) = (f *** g) >$< joinTNF p q
joinTNF (Transform f p) q = first f >$< joinTNF p q
joinTNF p (Transform g q) = second g >$< joinTNF p q
joinTNF p q = Join p q

-- |Convert a 'Free' to Transform Normal Form: extract and merge all the 'Transform', if any, to a single 'Transform' at the top.
freeTNF :: Free f a -> Free f a
freeTNF (Transform f p) = f >$< freeTNF p
freeTNF (Join p q) = joinTNF (freeTNF p) (freeTNF q)
freeTNF (Choose p q) = chooseTNF (freeTNF p) (freeTNF q)
freeTNF p = p

joinTDNF :: Free f a -> Free f b -> Free f (a, b)
joinTDNF (Transform f p) (Transform g q) = (f *** g) >$< joinTDNF p q
joinTDNF (Transform f p) q = first f >$< joinTDNF p q
joinTDNF p (Transform g q) = second g >$< joinTDNF p q
joinTDNF (Choose pp pq) q = Transform I.eitherFirst $ Choose (joinTDNF pp q) (joinTDNF pq q)
joinTDNF p (Choose qp qq) = Transform I.eitherSecond $ Choose (joinTDNF p qp) (joinTDNF p qq)
joinTDNF p Empty = Transform (I.invert I.fst) $ p
joinTDNF Empty q = Transform (I.invert I.snd) $ q
joinTDNF p q = Join p q

-- |Convert a 'Free' to Transform Disjunctive Normal Form: reorder the terms so thet at most one 'Transform' is on the outside, followed by 'Choose' terms, which are above all 'Join' terms', with 'Empty' and 'Free' as leaves.
-- Since each 'Join' above a 'Choose' creates a duplicate 'Join' term, the complexity and result size can be exponential (just as with boolean logic DNF).
freeTDNF :: Free f a -> Free f a
freeTDNF (Transform f p) = f >$< freeTDNF p
freeTDNF (Join p q) = joinTDNF (freeTDNF p) (freeTDNF q)
freeTDNF (Choose p q) = chooseTNF (freeTDNF p) (freeTDNF q)
freeTDNF p = p

chooseLinear :: Free f a -> Free f b -> Free f (Either a b)
chooseLinear (Choose p q) r = I.exchange >$< chooseLinear p (Choose q r)
chooseLinear p q = chooseTNF (freeLinear p) (freeLinear q)

joinLinear :: Free f a -> Free f b -> Free f (a, b)
joinLinear (Join p q) r = I.invert I.flatten2_1 I.. I.flatten1_2 >$< joinLinear p (Join q r)
joinLinear p q = joinTNF (freeLinear p) (freeLinear q)

freeLinear :: Free f a -> Free f a
freeLinear (Transform f p) = f >$< freeLinear p
freeLinear (Choose p q) = chooseLinear p q
freeLinear (Join p q) = joinLinear p q
freeLinear p = p

-- |Convert a 'Free' to Linearized Transform Disjunction Normal Form: same as 'freeTDNF' except that all 'Choose' and 'Join' trees are linearized to the right: @Tree (Tree a b) c@ turns into @Tree a (Tree b c)@.
freeLinearTDNF :: Free f a -> Free f a
freeLinearTDNF = freeLinear . freeTDNF
