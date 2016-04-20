-- |
-- Bidirectional functions.
-- The type '<->' is the most useful, representing a bijective function between two types.
-- Bijections can be constructed from two functions with '<->' or from a set of Haskell cases using 'biCase'.
--
-- This and other modules in this package export functionality generally mirroring that provided by the base modules, but over bijections.  They are thus intended to be imported qualified, e.g.,:
-- 
-- > import qualified Data.Bijection as Bi
--
module Data.Bijection
  ( module Control.BiArrow
  , module Data.Bijection.Type
  , module Data.Bijection.TH
  , module Data.Bijection.Prelude
  , module Data.Bijection.Bits
  , module Data.Bijection.Bool
  , module Data.Bijection.Coerce
  , module Data.Bijection.Complex
  , module Data.Bijection.Either
  , module Data.Bijection.Function
  , module Data.Bijection.Functor
  , module Data.Bijection.List
  , module Data.Bijection.Maybe
  , module Data.Bijection.Ord
  , module Data.Bijection.Tuple
  ) where

import Control.BiArrow
import Data.Bijection.Type
import Data.Bijection.TH
import Data.Bijection.Prelude
import Data.Bijection.Bits
import Data.Bijection.Bool
import Data.Bijection.Coerce
import Data.Bijection.Complex
import Data.Bijection.Either
import Data.Bijection.Function
import Data.Bijection.Functor
import Data.Bijection.List
import Data.Bijection.Maybe
import Data.Bijection.Ord
import Data.Bijection.Tuple
