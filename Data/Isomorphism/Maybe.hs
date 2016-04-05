-- |
-- Versions of functions from "Data.Maybe" as isomorphisms.
{-# LANGUAGE Safe #-}
module Data.Isomorphism.Maybe
  ( isJust
  , isNothing
  , listToMaybe
  , maybeToList
  , fromMaybe
  ) where

import qualified Data.Maybe as M

import Data.Isomorphism.Type
import Data.Isomorphism.TH
import Data.Isomorphism.Internal

-- |Convert between 'Just ()' and 'True' (see 'M.isJust').
isJust :: Maybe () <-> Bool
isJust =
  [isoCase|
    Just () <-> True
    Nothing <-> False
  |]

-- |Convert between 'Nothing' and 'True' (see 'M.isNothing'). (@'Data.Isomorphism.Bool.not' . 'isJust'@)
isNothing :: Maybe () <-> Bool
isNothing = 
  [isoCase|
    Nothing <-> True
    Just () <-> False
  |]

-- |Convert between (the head of) a (singleton) list and 'Maybe' (see 'M.listToMaybe'). (@'invert' 'maybeToList'@)
listToMaybe :: [a] <-> Maybe a
listToMaybe = M.listToMaybe :<->: M.maybeToList

-- |Convert between 'Maybe' and a (singleton) list (see 'M.maybeToList'). (@'invert' 'listToMaybe'@)
maybeToList :: Maybe a <-> [a]
maybeToList = invert listToMaybe

-- |Convert between 'Nothing' and a default value, or 'Just' and its value (not a true isomorphism).
fromMaybe :: Eq a => a -> Maybe a <-> a
fromMaybe d = M.fromMaybe d :<->: \a -> if a == d then Nothing else Just a
