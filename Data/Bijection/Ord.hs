-- |
-- Bidirectional operations over 'Ordering'.
{-# LANGUAGE Safe #-}
module Data.Bijection.Ord
  ( down
  ) where

import Data.Bijection.Type
import Data.Bijection.TH

-- |Invert an 'Ordering' (see 'Data.Ord.Down').
down :: Ordering <-> Ordering
down =
  [biCase|
    LT <-> GT
    EQ <-> EQ
    GT <-> LT
  |]
