-- |
-- Combine monoidal functors into HLists.
{-# LANGUAGE TypeOperators, DataKinds, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Control.Invertible.Monoidal.HList
  ( hConsI
  , (>:*<)
  , HSequenceI(..)
  ) where

import Prelude hiding (Functor(..), (<$>), fst, snd, id)
import qualified Data.HList as HL

import Data.Invertible.HList
import Control.Invertible.Monoidal

-- |'HL.HCons' two monoidal functors.
hConsI :: Monoidal f => f a -> f (HL.HList l) -> f (HL.HList (a ': l))
hConsI = liftI2 hCons

-- |Infix alias for 'hConsI'.
(>:*<) :: Monoidal f => f a -> f (HL.HList l) -> f (HL.HList (a ': l))
(>:*<) = hConsI

infixr 4 >:*<

-- |A monoidal version of 'HL.HSequence': a heteogeneous version of 'sequenceMaybesI'.
class (Monoidal m, HL.SameLength a b) => HSequenceI m a b | a -> b, m b -> a where
  hSequenceI :: HL.HList a -> m (HL.HList b)

instance Monoidal m => HSequenceI m '[] '[] where
  hSequenceI _ = pureI HL.HNil

instance (m1 ~ m, Monoidal m, HSequenceI m as bs) => HSequenceI m (m1 a ': as) (a ': bs) where
  hSequenceI (HL.HCons a b) = a >:*< hSequenceI b

