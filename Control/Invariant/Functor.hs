-- |
-- This provides a subset of the functionality as the invariant's "Data.Functor.Invariant" module, but based on "Data.Isomorphism", without all the instances, and with an interface matching "Data.Functor".
--
-- This module is intended to be imported qualified, e.g.,:
-- 
-- > import qualified Control.Invariant.Functor as Inv
--
{-# LANGUAGE Safe, FlexibleInstances #-}
module Control.Invariant.Functor
  ( Functor(..)
  , isofmap
  , (<$>)
  ) where

import Prelude hiding ((.), Functor(..), (<$>))

import Data.Isomorphism.Type
import Data.Isomorphism.Prelude (invert, (.))

-- |An invariant version of 'Data.Functor.Functor', equivalent to 'Data.Functor.Inviarant.Invariant'.
class Functor f where
  fmap :: a <-> b -> f a -> f b

-- |Lift an isomorphism over an invariant 'Functor'.
-- There is probably no need for this function.
isofmap :: Functor f => a <-> b -> f a <-> f b
isofmap f = fmap f :<->: fmap (invert f)

-- |An infix synnonym for 'fmap'.
(<$>) :: Functor f => a <-> b -> f a -> f b
(<$>) = fmap
infixl 4 <$>

instance Functor (Isomorphism (->) a) where
  fmap = (.)

