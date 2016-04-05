-- |
-- The base representation of an isomorphism.
{-# LANGUAGE Trustworthy, KindSignatures, FlexibleInstances, CPP #-}
module Data.Isomorphism.Type
  ( Isomorphism(..)
  , type (<->)
  ) where

import Prelude hiding (id, (.))
import Control.Category (Category(..))
#ifdef VERSION_invariant
import Data.Functor.Invariant (Invariant(..), Invariant2(..))
#endif

infix 2 <->, :<->:

-- |Isomorphism: an arrow and its inverse.
-- Most uses will prefer the specialized '<->' type for function arrows.
--
-- To constitute a valid (bijective) isomorphism, 'isoTo' and 'isoFrom' should be inverses:
--
-- > isoTo . isoFrom = id
-- > isoFrom . isoTo = id
data Isomorphism (a :: * -> * -> *) b c = (:<->:)
  { isoTo :: a b c
  , isoFrom :: a c b
  }

-- |Specialization of 'Isomorphism' to function arrows.
type (<->) = Isomorphism (->)

instance Category a => Category (Isomorphism a) where
  id = id :<->: id
  (f1 :<->: g1) . (f2 :<->: g2) = f1 . f2 :<->: g2 . g1

#ifdef VERSION_invariant
instance Invariant (Isomorphism (->) b) where
  invmap = ((.) .) . (:<->:)

instance Invariant2 (Isomorphism (->)) where
  invmap2 f g = (.) ((. (g :<->: f)) .) . invmap
#endif
