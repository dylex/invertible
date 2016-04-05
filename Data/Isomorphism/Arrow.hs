-- |
-- Versions of functions from "Control.Arrow" over isomorphisms.
module Data.Isomorphism.Arrow
  ( arr
  , first
  , second
  , (***)
  , left
  , right
  , (+++)
  ) where

import qualified Control.Arrow as A

import Data.Isomorphism.Type

-- |Lift a function isomorphism to an arbitrary arrow isomorphism using 'A.arr'.
arr :: A.Arrow a => (b <-> c) -> Isomorphism a b c
arr (f :<->: g) = A.arr f :<->: A.arr g

-- |Send the 'A.first' component of the input through the isomorphism, and keep the rest unchanged.
-- With function arrows, this has type @(a <-> b) -> ((a, c) <-> (b, c))@.
first :: A.Arrow a => Isomorphism a b c -> Isomorphism a (b, d) (c, d)
first (f :<->: g) = A.first f :<->: A.first g

-- |Send the 'A.second' component of the input through the isomorphism, and keep the rest unchanged.
-- With function arrows, this has type @(a <-> b) -> ((c, a) <-> (c, b))@.
second :: A.Arrow a => Isomorphism a b c -> Isomorphism a (d, b) (d, c)
second (f :<->: g) = A.second f :<->: A.second g

infixr 3 ***
-- |Split the input between the two argument isomorphism and combine their output using 'A.***'.
-- With function arrows, this has type @(a <-> b) -> (a' <-> b') -> ((a, a') <-> (b, b'))@.
(***) :: A.Arrow a => Isomorphism a b c -> Isomorphism a b' c' -> Isomorphism a (b, b') (c, c')
(f :<->: g) *** (f' :<->: g') = (f A.*** f') :<->: (g A.*** g')

-- |Feed 'A.left' inputs through the argument isomorphism, passing the rest through unchanged to the output.
-- With function arrows, this has type @(a <-> b) -> (Either a c <-> Either b c)@.
left :: A.ArrowChoice a => Isomorphism a b c -> Isomorphism a (Either b d) (Either c d)
left (f :<->: g) = A.left f :<->: A.left g

-- |Feed 'A.right' inputs through the argument isomorphism. A mirror image of 'left'.
-- With function arrows, this has type @(a <-> b) -> (Either c a <-> Either c b)@.
right :: A.ArrowChoice a => Isomorphism a b c -> Isomorphism a (Either d b) (Either d c)
right (f :<->: g) = A.right f :<->: A.right g

infixr 2 +++
-- |Split the input between the two argument arrows, retagging and merging their outputs using 'A.+++'.
-- With function arrows, this has type @(a <-> b) -> (a' <-> b') -> (Either a a' <-> Either b b')@.
(+++) :: A.ArrowChoice a => Isomorphism a b c -> Isomorphism a b' c' -> Isomorphism a (Either b b') (Either c c')
(f :<->: g) +++ (f' :<->: g') = (f A.+++ f') :<->: (g A.+++ g')

