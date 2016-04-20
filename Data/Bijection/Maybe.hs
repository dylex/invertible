-- |
-- Bidirectional version of "Data.Maybe".
{-# LANGUAGE Safe #-}
module Data.Bijection.Maybe
  ( isJust
  , isNothing
  , listToMaybe
  , maybeToList
  , fromMaybe
  ) where

import qualified Data.Maybe as M

import Data.Bijection.Type
import Data.Bijection.TH
import Data.Bijection.Internal

-- |Convert between 'Just ()' and 'True' (see 'M.isJust').
isJust :: Maybe () <-> Bool
isJust =
  [biCase|
    Just () <-> True
    Nothing <-> False
  |]

-- |Convert between 'Nothing' and 'True' (see 'M.isNothing'). (@'Data.Bijection.Bool.not' . 'isJust'@)
isNothing :: Maybe () <-> Bool
isNothing = 
  [biCase|
    Nothing <-> True
    Just () <-> False
  |]

-- |Convert between (the head of) a (singleton) list and 'Maybe' (see 'M.listToMaybe'). (@'invert' 'maybeToList'@)
listToMaybe :: [a] <-> Maybe a
listToMaybe = M.listToMaybe :<->: M.maybeToList

-- |Convert between 'Maybe' and a (singleton) list (see 'M.maybeToList'). (@'invert' 'listToMaybe'@)
maybeToList :: Maybe a <-> [a]
maybeToList = invert listToMaybe

-- |Convert between 'Nothing' and a default value, or 'Just' and its value (not a true bijection).
fromMaybe :: Eq a => a -> Maybe a <-> a
fromMaybe d = M.fromMaybe d :<->: \a -> if a == d then Nothing else Just a
