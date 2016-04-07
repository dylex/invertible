-- |
-- Isomorphisms.
-- The type '<->' is the most useful, representing an isomorphism between two types.
-- Isomorphisms can be constructed from two functions with ':<->:' or from a set of Haskell cases using 'isoCase'.
--
-- This and other modules in this package export functionality generally mirroring that provided by the base modules, but over isomorphisms.  They are thus intended to be imported qualified, e.g.,:
-- 
-- > import qualified Data.Isomorphism as Iso
--
module Data.Isomorphism
  ( module Data.Isomorphism.Type
  , module Data.Isomorphism.TH
  , module Data.Isomorphism.Prelude
  , module Data.Isomorphism.Arrow
  , module Data.Isomorphism.Bits
  , module Data.Isomorphism.Bool
  , module Data.Isomorphism.Coerce
  , module Data.Isomorphism.Complex
  , module Data.Isomorphism.Either
  , module Data.Isomorphism.Function
  , module Data.Isomorphism.Functor
  , module Data.Isomorphism.List
  , module Data.Isomorphism.Maybe
  , module Data.Isomorphism.Ord
  , module Data.Isomorphism.Tuple
  ) where

import Data.Isomorphism.Type
import Data.Isomorphism.TH
import Data.Isomorphism.Prelude
import Data.Isomorphism.Arrow
import Data.Isomorphism.Bits
import Data.Isomorphism.Bool
import Data.Isomorphism.Coerce
import Data.Isomorphism.Complex
import Data.Isomorphism.Either
import Data.Isomorphism.Function
import Data.Isomorphism.Functor
import Data.Isomorphism.List
import Data.Isomorphism.Maybe
import Data.Isomorphism.Ord
import Data.Isomorphism.Tuple
