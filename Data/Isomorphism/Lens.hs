-- |
-- Convert to and from lens isomorphisms in "Control.Lens.Iso".
{-# LANGUAGE RankNTypes #-}
module Data.Isomorphism.Lens
  ( toIso
  , unIso
  , isoIso
  ) where

import Data.Isomorphism.Type
import qualified Control.Lens.Iso as L

-- |Convert an isomorphism to a lens.
toIso :: a <-> b -> L.Iso' a b
toIso (f :<->: g) = L.iso f g

-- |Convert a lens to an isomorphism.
unIso :: L.AnIso' a b -> a <-> b
unIso l = L.withIso l (:<->:)

-- |As isomorphism between isomorphisms.
-- There is probably no need for this function.
isoIso :: (a <-> b) <-> L.AnIso' a b
isoIso = toIso :<->: unIso
