-- |
-- This provides a subset of the functionality as the invariant's "Data.Functor.Invariant" module, but based on "Data.Bijection", without all the instances, and with an interface matching "Data.Functor".
--
-- This module is intended to be imported qualified, e.g.,:
-- 
-- > import qualified Control.Invariant.Functor as Inv
--
{-# LANGUAGE Safe, FlexibleInstances #-}
module Control.Invariant.Functor
  ( Functor(..)
  , (<$>)
  ) where

import Prelude hiding ((.), Functor(..), (<$>))

import Data.Bijection.Type
import Data.Bijection.Prelude ((.))

-- |An invariant version of 'Data.Functor.Functor', equivalent to 'Data.Functor.Inviarant.Invariant'.
class Functor f where
  fmap :: a <-> b -> f a -> f b

-- |An infix synnonym for 'fmap'.
(<$>) :: Functor f => a <-> b -> f a -> f b
(<$>) = fmap
infixl 4 <$>

instance Functor (Bijection (->) a) where
  fmap = (.)

