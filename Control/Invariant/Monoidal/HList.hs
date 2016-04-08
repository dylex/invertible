-- |
-- Combine monoidal functors into HLists.
{-# LANGUAGE DataKinds #-}
module Control.Invariant.Monoidal.HList
  ( (>:*<)
  ) where

import Prelude hiding (Functor(..), (<$>), fst, snd, id)
import qualified Data.HList.HList as HL

import Data.Isomorphism.HList
import Control.Invariant.Monoidal

-- |'HL.HCons' two monoidal functors.
(>:*<) :: Monoidal f => f a -> f (HL.HList l) -> f (HL.HList (a ': l))
(>:*<) = liftI2 hCons

infixr 5 >:*<
