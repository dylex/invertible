-- |
-- Convert to and from semigroupoids 'Iso'.
module Data.Isomorphism.Semigroupoid
  ( toIso
  , fromIso
  , isoIso
  ) where

import Data.Isomorphism.Type
import qualified Data.Semigroupoid.Isomorphism as S

-- |Convert an isomorphism to semigroupoid form.
toIso :: Isomorphism a b c -> S.Iso a b c
toIso (f :<->: g) = S.Iso f g

-- |Convert semigroupoid form to an isomorphism.
fromIso :: S.Iso a b c -> Isomorphism a b c
fromIso (S.Iso f g) = f :<->: g

-- |As isomorphism between isomorphisms.
-- There is probably no need for this function.
isoIso :: (Isomorphism a b c) <-> S.Iso a b c
isoIso = toIso :<->: fromIso
