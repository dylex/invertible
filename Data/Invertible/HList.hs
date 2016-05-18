-- |
-- Bidirectional version of "Data.HList.HList".
{-# LANGUAGE DataKinds, QuasiQuotes, TypeOperators, FlexibleContexts #-}
module Data.Invertible.HList
  ( hCons
  , hReverse
  , hReverse_
  , hAppend
  , hZip
  ) where

import qualified Data.HList as HL
import Data.Proxy (Proxy(..))

import Data.Invertible.Bijection
import Data.Invertible.TH

-- |(De)construct an list from a head and tail.
hCons :: (a, HL.HList l) <-> HL.HList (a ': l)
hCons = [biCase|(a, l) <-> HL.HCons a l|]

-- |'HL.hReverse' the order of a list.
hReverse :: (HL.HReverse a b, HL.HReverse b a) => HL.HList a <-> HL.HList b
hReverse = HL.hReverse :<->: HL.hReverse

-- |'HL.hReverse_' the order of a list.
hReverse_ :: (HL.HRevApp a '[] b, HL.HRevApp b '[] a) => HL.HList a <-> HL.HList b
hReverse_ = HL.hReverse_ :<->: HL.hReverse_

-- |'HL.hAppend' (concatenate) or split two lists.
hAppend :: (HL.HAppendList a b, HL.HSplitAt n (HL.HAppendListR a b) a b) => (HL.HList a, HL.HList b) <-> HL.HList (HL.HAppendListR a b)
hAppend = uncurry HL.hAppendList :<->: HL.hSplitAt Proxy

-- |'HL.hZip' two lists together.
hZip :: HL.HZipList a b l => (HL.HList a, HL.HList b) <-> HL.HList l
hZip = uncurry HL.hZipList :<->: HL.hUnzipList
