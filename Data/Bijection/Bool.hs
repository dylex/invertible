-- |
-- Bidirectional version of "Data.Bool".
{-# LANGUAGE Safe #-}
module Data.Bijection.Bool
  ( not
  ) where

import Prelude hiding (not)
import qualified Data.Bool as B

import Data.Bijection.Type
import Data.Bijection.Internal

-- |Boolean 'B.not'.
not :: Bool <-> Bool
not = involution B.not
