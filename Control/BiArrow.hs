-- |
-- Bidirectional arrows.
-- Taken directly from
--
--  * Artem Alimarine, et al. /There and Back Again: Arrows for Invertible Programming/. Haskell '05. <http://citeseer.ist.psu.edu/alimarine05there.html>
--
module Control.BiArrow
  ( BiArrow(..)
  , biarr
  , involve
  , (^^>>)
  , (>>^^)
  , (<<^^)
  , (^^<<)
  ) where

import Control.Arrow

import Data.Bijection.Type

infix 2 <->

-- |The bidirectional arrow class.
--
-- Instances should satisfy the following laws:
--
--  * @f1 \<-\> g2 >>> g1 \<-\> f2 = (f1 >>> g1) \<-\> (f2 >>> g2)@
--  * @first (f \<-\> g) = f *** id \<-\> g *** id@
--  * @first h >>> id *** f \<-\> id *** g = id *** f \<-\> id *** g >>> first h@
--  * @first (first f) >>> assoc = assoc >>> first f@
--
-- where @assoc = ['Data.Bijection.TH.biCase'|((x,y),z) \<-\> (x,(y,z))|]@
--
-- Although this is not, strictly speaking, a subclass of 'Arrow' as it is often impossible to define 'arr'), this is done in the paper because \"conceptually bi-arrows form an extension of the arrow class. Moreover, it allows us to use bi-arrows as normal arrows.\"
class Arrow a => BiArrow a where
  -- |Take two functions and lift them into a bidirectional arrow.
  -- The intention is that these functions are each other's inverse.
  (<->) :: (b -> c) -> (c -> b) -> a b c
  -- |Inverse: reverse the direction of a bidirectional arrow.
  inv :: a b c -> a c b

instance Arrow a => BiArrow (Bijection a) where
  f <-> g = arr f :<->: arr g
  inv (f :<->: g) = g :<->: f

-- |Lift a bidirectional function to an arbitrary arrow using '<->'.
biarr :: BiArrow a => (b <-> c) -> a b c
biarr (f :<->: g) = f <-> g

-- |Construct an involution (a biarrow where the function and inverse are the same).
involve :: BiArrow a => (b -> b) -> a b b
involve f = f <-> f

infixr 1 ^^>>, >>^^
infixr 1 ^^<<, <<^^

-- | Precomposition with a pure bijection.
(^^>>) :: BiArrow a => (b <-> c) -> a c d -> a b d
f ^^>> a = biarr f >>> a

-- | Postcomposition with a pure bijection.
(>>^^) :: BiArrow a => a b c -> (c <-> d) -> a b d
a >>^^ f = a >>> biarr f

-- | Precomposition with a pure bijection (right-to-left variant).
(<<^^) :: BiArrow a => a c d -> (b <-> c) -> a b d
a <<^^ f = a <<< biarr f

-- | Postcomposition with a pure bijection (right-to-left variant).
(^^<<) :: BiArrow a => (c <-> d) -> a b c -> a b d
f ^^<< a = biarr f <<< a
