-- |
-- Versions of functions from "Data.Complex" as isomorphisms.
{-# LANGUAGE Safe #-}
module Data.Isomorphism.Complex
  ( complex
  , polar
  , conjugate
  ) where

import qualified Data.Complex as C

import Data.Isomorphism.Type
import Data.Isomorphism.Internal
import Data.Isomorphism.TH

-- |Convert between 'Complex' numbers and rectangular form.
complex :: (a, a) <-> C.Complex a
complex = [isoCase|(r, i) <-> r C.:+ i|]

-- |Convert between complex numbers and 'C.polar' form.
polar :: RealFloat a => (a, a) <-> C.Complex a
polar = uncurry C.mkPolar :<->: C.polar

-- |The 'C.conjugate' of a complex number.
conjugate :: Num a => C.Complex a <-> C.Complex a
conjugate = involution C.conjugate
