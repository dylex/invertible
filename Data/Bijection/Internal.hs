{-# LANGUAGE Safe #-}
module Data.Bijection.Internal
  ( invert
  , involution
  ) where

import Data.Bijection.Type

-- |Invert an isomorphism by exchanging the two arrows (same as 'Control.BiArrow.inv').
invert :: Bijection a b c -> Bijection a c b
invert (f :<->: g) = g :<->: f
{-# INLINE invert #-}

-- |Construct an involution (an bijection where the function and inverse are the same) (same as 'Control.BiArrow.involve').
involution :: a b b -> Bijection a b b
involution f = f :<->: f
{-# INLINE involution #-}
