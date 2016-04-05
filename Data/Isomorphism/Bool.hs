-- |
-- Versions of functions from "Data.Bool" as isomorphisms.
module Data.Isomorphism.Bool
  ( not
  ) where

import Prelude hiding (not)
import qualified Data.Bool as B

import Data.Isomorphism.Type
import Data.Isomorphism.Internal

-- |Boolean 'B.not'.
not :: Bool <-> Bool
not = involution B.not
