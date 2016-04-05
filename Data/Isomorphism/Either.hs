-- |
-- Versions of functions from "Data.Either" as isomorphisms.
module Data.Isomorphism.Either
  ( switch
  , isLeft
  , isRight
  , lft
  , rgt
  ) where

import Prelude

import Data.Isomorphism.Type
import Data.Isomorphism.TH

-- |Convert between 'Left' and 'Right'.
switch :: Either a b <-> Either b a
switch =
  [isoCase|
    Left a <-> Right a
    Right a <-> Left a
  |]

-- |Convert between 'Left' and 'True' (see 'Data.Either.isLeft').
isLeft :: Either () () <-> Bool
isLeft =
  [isoCase|
    Left () <-> True
    Right () <-> False
  |]

-- |Convert between 'Right' and 'True' (see 'Data.Either.isRight'). (@'Data.Isomorphism.Bool.not' . 'isLeft'@) 
isRight :: Either () () <-> Bool
isRight =
  [isoCase|
    Right () <-> True
    Left () <-> False
  |]

-- |Convert between 'Left' and 'Just'.
lft :: Either a () <-> Maybe a
lft =
  [isoCase|
    Left a <-> Just a
    Right () <-> Nothing
  |]

-- |Convert between 'Right and 'Just'.
rgt :: Either () a <-> Maybe a
rgt =
  [isoCase|
    Left () <-> Nothing
    Right a <-> Just a
  |]
