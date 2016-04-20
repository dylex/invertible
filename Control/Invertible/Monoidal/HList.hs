-- |
-- Combine monoidal functors into HLists.
{-# LANGUAGE DataKinds #-}
module Control.Invertible.Monoidal.HList
  ( (>:*<)
  ) where

import Prelude hiding (Functor(..), (<$>), fst, snd, id)
import qualified Data.HList.HList as HL

import Data.Invertible.HList
import Control.Invertible.Monoidal

-- |'HL.HCons' two monoidal functors.
(>:*<) :: Monoidal f => f a -> f (HL.HList l) -> f (HL.HList (a ': l))
(>:*<) = liftI2 hCons

infixr 4 >:*<
