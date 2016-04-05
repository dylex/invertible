-- |
-- Isomorphisms that operate over 'Ordering'.
module Data.Isomorphism.Ord
  ( down
  ) where

import Data.Isomorphism.Type
import Data.Isomorphism.TH

-- |Invert an 'Ordering' (see 'Data.Ord.Down').
down :: Ordering <-> Ordering
down =
  [isoCase|
    LT <-> GT
    EQ <-> EQ
    GT <-> LT
  |]
