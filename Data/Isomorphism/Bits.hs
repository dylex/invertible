-- |
-- Versions of functions from "Data.Bits" as isomorphisms.
module Data.Isomorphism.Bits
  ( complement
  ) where

import qualified Data.Bits as B

import Data.Isomorphism.Type
import Data.Isomorphism.Internal

-- |'B.complement' all the bits in the argument.
complement :: B.Bits a => a <-> a
complement = involution B.complement
