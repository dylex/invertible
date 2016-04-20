-- |
-- Bidirectional version of "Data.Functor".
{-# LANGUAGE Safe #-}
module Data.Bijection.Functor
  ( fmap
  , (<$>)
  , identity
  ) where

import Prelude hiding (fmap, (<$>))
import qualified Data.Functor as F
import Data.Functor.Identity (Identity(..))

import Data.Bijection.Type
import Data.Bijection.TH

-- |Lift both sides of an bijection over a functor using 'F.fmap'.
fmap :: Functor f => a <-> b -> f a <-> f b
fmap (f :<->: g) = F.fmap f :<->: F.fmap g

-- |An infix synnonym for 'fmap'.
(<$>) :: Functor f => a <-> b -> f a <-> f b
(<$>) = fmap
infixl 4 <$>

-- |Convert the 'Identity' functor.
identity :: a <-> Identity a
identity = [biCase|a <-> Identity a|]
