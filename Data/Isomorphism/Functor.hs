-- |
-- Versions of functions from "Data.Functor" over isomorphisms.
{-# LANGUAGE Safe #-}
module Data.Isomorphism.Functor
  ( fmap
  , (<$>)
  , identity
  ) where

import Prelude hiding (fmap, (<$>))
import qualified Data.Functor as F
import Data.Functor.Identity (Identity(..))

import Data.Isomorphism.Type
import Data.Isomorphism.TH

-- |Lift both sides of an isomorphism over a functor using 'F.fmap'.
fmap :: Functor f => a <-> b -> f a <-> f b
fmap (f :<->: g) = F.fmap f :<->: F.fmap g

-- |An infix synnonym for 'fmap'.
(<$>) :: Functor f => a <-> b -> f a <-> f b
(<$>) = fmap
infixl 4 <$>

-- |Convert the 'Identity' functor.
identity :: a <-> Identity a
identity = [isoCase|a <-> Identity a|]
