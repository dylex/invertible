-- |
-- Bidirectional version of "Data.Bits".
{-# LANGUAGE Safe #-}
module Data.Bijection.Bits
  ( complement
  ) where

import qualified Data.Bits as B

import Data.Bijection.Type
import Data.Bijection.Internal

-- |'B.complement' all the bits in the argument.
complement :: B.Bits a => a <-> a
complement = involution B.complement
