-- |
-- Isomorphisms that operate over lists.
module Data.Isomorphism.List
  ( cons
  , uncons
  , reverse
  , transpose
  , zip
  , lines
  , words
  ) where

import Prelude hiding (reverse, zip, unzip, lines, words)
import qualified Data.List as L

import Data.Isomorphism.Type
import Data.Isomorphism.TH
import Data.Isomorphism.Internal

-- |Convert between @'Just' (head, tail)@ and the non-empty list @head:tail@.
cons :: Maybe (a, [a]) <-> [a]
cons =
  [isoCase|
    Just (a, l) <-> a:l
    Nothing <-> []
  |]

-- |Convert between the non-empty list @head:tail@ and @'Just' (head, tail)@. (@'invert' 'cons'@)
uncons :: [a] <-> Maybe (a, [a])
uncons = invert cons

-- |'L.reverse' the order of a (finite) list.
reverse :: [a] <-> [a]
reverse = involution L.reverse

-- |'L.transpose' the rows and columns of its argument.
transpose :: [[a]] <-> [[a]]
transpose = involution L.transpose

-- |'L.zip' two lists together.
zip :: ([a], [b]) <-> [(a, b)]
zip = uncurry L.zip :<->: L.unzip

-- |Split a string into 'L.lines'.
lines :: String <-> [String]
lines = L.lines :<->: L.unlines

-- |Split a string into 'L.words'.
words :: String <-> [String]
words = L.words :<->: L.unwords
