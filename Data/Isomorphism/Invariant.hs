-- |
-- Use isomorphisms with 'Invariant' functors from "Data.Functor.Invariant".
module Data.Isomorphism.Invariant
  ( invmap
  , (<$>)
  , isomap
  ) where

import Prelude hiding ((<$>))

import Data.Isomorphism.Type
import qualified Data.Functor.Invariant as I

-- |Apply an isomorphism over an 'Invariant' using 'I.invmap'.
invmap :: I.Invariant f => a <-> b -> f a -> f b
invmap (f :<->: g) = I.invmap f g

-- |An infix synnonym for 'invmap'.
-- Although it may make sense for this to be 'Data.Isomorphism.Functor.fmap' instead, this is likely the most commonly used function, and is in some way a better analogue, as it applies an isomorphism over an invariant (rather than a function over a functor).
(<$>) :: I.Invariant f => a <-> b -> f a -> f b
(<$>) = invmap
infixl 4 <$>

-- |Lift an isomorphism over an 'Invariant' functor.
-- There is probably no need for this function.
isomap :: I.Invariant f => a <-> b -> f a <-> f b
isomap (f :<->: g) = I.invmap f g :<->: I.invmap g f
