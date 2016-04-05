module Data.Isomorphism.Internal
  ( involution
  , invert
  , isoInvert
  ) where

import Data.Isomorphism.Type

-- |Construct an involution (an isomorphism where the function and inverse are the same).
involution :: a b b -> Isomorphism a b b
involution f = f :<->: f
{-# INLINE involution #-}

-- |Invert an isomorphism by exchanging the two arrows.
invert :: Isomorphism a b c -> Isomorphism a c b
invert (f :<->: g) = g :<->: f
{-# INLINE invert #-}

-- |An isomorphism of inversions.
-- There is probably no need for this function.
isoInvert :: Isomorphism a b c <-> Isomorphism a c b
isoInvert = invert :<->: invert
