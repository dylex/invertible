{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Conduit.Arrow
  ( ArrConduit(..)
  , ArrProduce
  , ArrSource
  , ArrConsume(..)
  , ArrSink
  ) where

import           Control.Arrow (Arrow(..), ArrowZero(..), ArrowPlus(..), ArrowChoice(..), Kleisli)
import qualified Control.Category as Cat
import           Control.Monad (ap)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Conduit
import qualified Data.Conduit.Internal as C
import qualified Data.Conduit.List as CL
import           Data.Void (Void)

newtype ArrConduit   m a b = ArrConduit{ arrConduit :: Conduit a m b }

instance Monad m => Functor (ArrConduit m a) where
  fmap f (ArrConduit c) = ArrConduit $ mapOutput f c

instance Monad m => Applicative (ArrConduit m a) where
  pure = ArrConduit . yield
  (<*>) = ap

instance Monad m => Monad (ArrConduit m a) where
  -- second conduit gets no input...
  ArrConduit c >>= f = ArrConduit $ c .| awaitForever ((return () .|) . arrConduit . f)

instance Monad m => Cat.Category (ArrConduit m) where
  id = ArrConduit $ awaitForever yield
  ArrConduit f . ArrConduit g = ArrConduit $ fuse g f

data ArrConduitState s i
  = ArrConduitStart -- ^nothing read yet
  | ArrConduitInput { arrConduitState' :: s, _arrConduitInput :: i } -- ^read input, not yet passed on
  | ArrConduitOutput{ arrConduitState' :: s } -- ^read and passed on input
  | ArrConduitEnd   { arrConduitState' :: s } -- ^read EOF

arrConduitState :: ArrConduitState s i -> Maybe s
arrConduitState ArrConduitStart = Nothing
arrConduitState s = Just $ arrConduitState' s

instance Monad m => Arrow (ArrConduit m) where
  arr = ArrConduit . CL.map
  first (ArrConduit (C.ConduitM c0)) = ArrConduit $ C.ConduitM $ \rest -> let
    go _ (C.Done _) = rest ()
    go s (C.PipeM m) = C.PipeM (go s <$> m)
    go s (C.NeedInput f e) = C.NeedInput
      (\(a, b) -> go (ArrConduitOutput b) $ f a)
      (maybe rest (\b -> go (ArrConduitEnd b) . e) $ arrConduitState s)
    go ArrConduitStart p = C.NeedInput
      (\(a, b) -> go (ArrConduitInput b a) p)
      rest
    go s (C.Leftover p i) = C.Leftover (go s p) (i, arrConduitState' s)
    go s (C.HaveOutput p f o) = C.HaveOutput (go s p) f (o, arrConduitState' s)
    in go ArrConduitStart (c0 C.Done)
  second (ArrConduit (C.ConduitM c0)) = ArrConduit $ C.ConduitM $ \rest -> let
    go _ (C.Done _) = rest ()
    go s (C.PipeM m) = C.PipeM (go s <$> m)
    go s (C.NeedInput f e) = C.NeedInput
      (\(a, b) -> go (ArrConduitOutput a) $ f b)
      (maybe rest (\b -> go (ArrConduitEnd b) . e) $ arrConduitState s)
    go ArrConduitStart p = C.NeedInput
      (\(a, b) -> go (ArrConduitInput a b) p)
      rest
    go s (C.Leftover p i) = C.Leftover (go s p) (arrConduitState' s, i)
    go s (C.HaveOutput p f o) = C.HaveOutput (go s p) f (arrConduitState' s, o)
    in go ArrConduitStart (c0 C.Done)
  f *** g = first f Cat.>>> second g

instance Monad m => ArrowZero (ArrConduit m) where
  zeroArrow = ArrConduit $ return ()

instance Monad m => ArrowPlus (ArrConduit m) where
  ArrConduit f <+> ArrConduit g = ArrConduit $ f >> g

instance Monad m => ArrowChoice (ArrConduit m) where
  ArrConduit (C.ConduitM l0) +++ ArrConduit (C.ConduitM r0) = ArrConduit $ C.ConduitM $ \rest -> let
    go _ (C.Done _) (C.Done _)  = rest ()
    go e (C.PipeM l) r          = C.PipeM      (og e r <$> l)
    go e (C.HaveOutput l f o) r = C.HaveOutput (go e l r) f (Left o)
    go e (C.Leftover l i) r     = C.Leftover   (go e l r)   (Left i)
    go e l (C.PipeM r)          = C.PipeM      (go e l <$> r)
    go e l (C.HaveOutput r f o) = C.HaveOutput (go e l r) f (Right o)
    go e l (C.Leftover r i)     = C.Leftover   (go e l r)   (Right i)
    go False l r = C.NeedInput
      (either
        (og False r . feed l)
        (go False l . feed r))
      (\e -> go True
        (end l e)
        (end r e))
    go True _ _ = rest ()
    og = flip . go
    feed (C.NeedInput f _) x = f x
    feed p _ = p
    end (C.NeedInput _ e) x = e x
    end p _ = p
    in go False (l0 C.Done) (r0 C.Done)

-- newtype ArrProduce i m a b = ArrProduce{ arrProduce :: a -> Conduit i m b }
type ArrProduce i m = Kleisli (ArrConduit m i)
type ArrSource m = ArrProduce () m

newtype ArrConsume o m a b = ArrConsume{ arrConsume :: MaybeT (ConduitM a o m) b }
  deriving (Functor, Applicative, Monad) -- MonadThrow, ...
type ArrSink = ArrConsume Void

instance Monad m => Cat.Category (ArrConsume o m) where
  id = ArrConsume $ MaybeT await
  ArrConsume _f . ArrConsume _g = error "TODO"
