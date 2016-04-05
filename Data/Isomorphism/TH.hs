-- |
-- Convenient construction of function isomorphisms using case-like syntax.
{-# LANGUAGE TemplateHaskell, Trustworthy #-}
module Data.Isomorphism.TH
  ( isoCase
  ) where

import Control.Arrow (second)
import Control.Monad (liftM2)
import Data.Char (isSpace)
import Data.Data (Data, gmapT)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Typeable (cast)
import Language.Haskell.Meta.Parse (parsePat)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Isomorphism.Type

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split t s@(c:r)
  | Just s' <- stripPrefix t s = [] : split t s'
  | p:l <- split t r = (c:p):l
  | otherwise = [s]

patToPat :: TH.Pat -> TH.Pat
patToPat = ptp . gmapT pta where
  pta :: Data a => a -> a
  pta = fromMaybe id $ cast patToPat
  ptp (TH.ViewP e p) = TH.ViewP (TH.VarE 'isoTo `TH.AppE` e) p
  ptp p = p

patToExp :: TH.Pat -> TH.Exp
patToExp (TH.LitP l) = TH.LitE l
patToExp (TH.VarP v) = TH.VarE v
patToExp (TH.TupP l) = TH.TupE $ map patToExp l
patToExp (TH.UnboxedTupP l) = TH.UnboxedTupE $ map patToExp l
patToExp (TH.ConP c a) = foldl (\f -> TH.AppE f . patToExp) (TH.ConE c) a
patToExp (TH.InfixP l o r) = TH.InfixE (Just $ patToExp l) (TH.ConE o) (Just $ patToExp r)
patToExp (TH.UInfixP l o r) = TH.UInfixE (patToExp l) (TH.ConE o) (patToExp r)
patToExp (TH.ParensP p) = TH.ParensE $ patToExp p
patToExp (TH.TildeP p) = patToExp p
patToExp (TH.BangP p) = patToExp p
patToExp (TH.AsP _ p) = patToExp p
patToExp TH.WildP = TH.VarE 'undefined
patToExp (TH.RecP c f) = TH.RecConE c $ map (second patToExp) f
patToExp (TH.ListP l) = TH.ListE $ map patToExp l
patToExp (TH.SigP p t) = TH.SigE (patToExp p) t
patToExp (TH.ViewP e p) = TH.VarE 'isoFrom `TH.AppE` e `TH.AppE` patToExp p

parseP :: String -> TH.PatQ
parseP s = either (fail . (++) ("Failed to parse pattern '" ++ s ++ "': ")) return $ parsePat s

isoExp :: String -> TH.ExpQ
isoExp = fmap ie . mapM ic . filter (not . all isSpace) . concatMap (split ";") . lines where
  ie l = TH.InfixE (Just $ ce l) (TH.ConE '(:<->:)) (Just $ ce $ map swap l)
  ce [(p, e)] = TH.LamE [patToPat p] $ patToExp e
  ce l = TH.LamCaseE [ TH.Match (patToPat p) (TH.NormalB $ patToExp e) [] | (p, e) <- l ]
  ic s
    | [fs, gs] <- split "<->" s =
      liftM2 (,) (parseP fs) (parseP gs)
    | otherwise = fail "each iso case must contain exactly one '<->'"

-- |Construct an expression representing an isomorphism based on a set of newline- or semicolon-separated cases.
-- Each case should be two pattern-expressions separated by @<->@.
-- Each pattern-expression is a haskell pattern that can also be interpreted as an expression.
-- You can think of these as symmetric or bi-directional case expressions.
-- The result will be an isomorphism that is the combination of two lambdas, one with the cases intepreted forward, and one reverse.
-- For example:
--
-- > newtype T a = C a
-- > isoC :: T a <-> a
-- > isoC = [isoCase| C a <-> a |]
--
-- > isJust :: Maybe () <-> Bool
-- > isJust = [isoCase|
-- >     Just () <-> True
-- >     Nothing <-> False
-- >   |]
-- 
isoCase :: QuasiQuoter
isoCase = QuasiQuoter
  { quoteExp = isoExp
  , quoteType = const $ fail "iso not supported in types"
  , quotePat = const $ fail "iso not supported in patterns"
  , quoteDec = const $ fail "iso not supported in declarations"
  }
