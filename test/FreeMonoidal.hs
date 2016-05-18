{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances, TypeOperators, TupleSections, ConstraintKinds #-}
module FreeMonoidal (tests) where

import Data.Functor.Classes (Show1)
import qualified Test.QuickCheck as Q

import Control.Invertible.Monoidal.Free
import qualified Data.Invertible as I

squelch :: Free f a -> Free f ()
squelch = Transform $ I.consts undefined ()

unfst :: a I.<-> (a, b)
unfst = (, undefined) I.:<->: fst

unsnd :: b I.<-> (a, b)
unsnd = (undefined, ) I.:<->: snd

unlft :: a I.<-> Either a b
unlft = Left I.:<->: either id undefined

unrgt :: b I.<-> Either a b
unrgt = Right I.:<->: either undefined id

instance Q.Arbitrary (f ()) => Q.Arbitrary (Free f ()) where
  arbitrary = Q.oneof
    [ return $ Empty
    , Free <$> Q.arbitrary
    , Transform <$> tupl <*> (Join   <$> Q.arbitrary <*> Q.arbitrary)
    , Transform <$> eith <*> (Choose <$> Q.arbitrary <*> Q.arbitrary)
    , Transform I.id <$> Q.arbitrary
    ] where
    tupl = Q.elements [I.invert unfst, I.invert unsnd]
    eith = (I.:<->:) both <$> Q.elements [Left, Right]
    both (Left x) = x
    both (Right x) = x

  shrink Empty = []
  shrink (Free f) = Empty : map Free (Q.shrink f) 
  shrink (Transform _ Empty) = [Empty]
  shrink (Transform _ (Free _)) = [Empty]
  shrink (Transform f (Join   p q)) =
    [Empty, pt, qt] ++ map (squelch . uncurry Join) (Q.shrink (pt, qt))
    where
    pt = Transform (f I.. unfst) p
    qt = Transform (f I.. unsnd) q
  shrink (Transform f (Choose p q)) =
    [Empty, pt, qt] ++ map (squelch . uncurry Choose) (Q.shrink (pt, qt))
    where
    pt = Transform (f I.. unlft) p
    qt = Transform (f I.. unrgt) q
  shrink (Transform f (Transform g p)) = Q.shrink $ Transform (f I.. g) p

type ShowFree f = (Functor f, Show1 f)

ok :: Q.Property
ok = Q.property True

bad :: ShowFree f => Free f a -> Q.Property
bad f = Q.counterexample (show f) False

checkNoT :: ShowFree f => Free f a -> Q.Property
checkNoT t@(Transform _ _) = bad t
checkNoT (Join p q) = checkNoT p Q..&&. checkNoT q
checkNoT (Choose p q) = checkNoT p Q..&&. checkNoT q
checkNoT _ = ok

checkTNF :: ShowFree f => Free f a -> Q.Property
checkTNF (Transform _ p) = checkNoT p
checkTNF p = checkNoT p

joinDNF :: ShowFree f => Free f a -> Q.Property
joinDNF t@(Transform _ _) = bad t
joinDNF c@(Choose _ _) = bad c
joinDNF (Join p q) = joinDNF p Q..&&. joinDNF q
joinDNF _ = ok

checkDNF :: ShowFree f => Free f a -> Q.Property
checkDNF t@(Transform _ _) = bad t
checkDNF (Choose p q) = checkDNF p Q..&&. checkDNF q
checkDNF p = joinDNF p

checkTDNF :: ShowFree f => Free f a -> Q.Property
checkTDNF (Transform _ p) = checkDNF p
checkTDNF p = checkDNF p

type WithInt = (,) Int
type FreeWithInt = Free WithInt ()

tests :: Q.Property
tests = Q.conjoin
  [ Q.label "TNF" . checkTNF . freeTNF :: FreeWithInt -> Q.Property
  , Q.label "TDNF" . checkTDNF . freeTDNF
  ]
