-- |Various 'Arrow' instances for "Data.Conduit"s.
-- The newtype wrappers here are somewhat similar to the types provided by old conduit versions (pre-0.4).
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

import           Control.Applicative (liftA2)
import           Control.Arrow (Arrow(..), ArrowZero(..), ArrowPlus(..), ArrowChoice(..), Kleisli)
import qualified Control.Category as Cat
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Conduit
import qualified Data.Conduit.Internal as C
import qualified Data.Conduit.List as CL
import           Data.Void (Void, absurd)

nop :: Applicative a => a ()
nop = pure ()

feedInput :: C.Pipe l i o u m r -> Either u i -> C.Pipe l i o u m r
feedInput (C.NeedInput f e) = either e f
feedInput p = const p

needInput :: (Either u i -> C.Pipe l i o u m r) -> C.Pipe l i o u m r
needInput f = C.NeedInput (f . Right) (f . Left)

-- |A 'Conduit' represented as an arrow from input stream to output stream.
newtype ArrConduit   m a b = ArrConduit{ arrConduit :: Conduit a m b }

instance Monad m => Functor (ArrConduit m a) where
  fmap f (ArrConduit c) = ArrConduit $ mapOutput f c

instance Monad m => Applicative (ArrConduit m a) where
  pure = ArrConduit . yield
  -- (<*>) = ap
  -- |Functions from the first conduit are applied greedily to output from the second conduit.
  -- There is a slight left bias, where input is provided to the function conduit first.
  ArrConduit (C.ConduitM a0) <*> ArrConduit (C.ConduitM b0) = ArrConduit $ C.ConduitM $ \rest -> let
    go z _ (C.Done _) (C.Done _) = C.PipeM (rest <$> z)
    go z s (C.PipeM a) b = C.PipeM (og z s b <$> a)
    go z s a (C.PipeM b) = C.PipeM (go z s a <$> b)
    go z _ (C.HaveOutput a z' s) b = go (z >> z') (Just s) a b
    go z (Just s) a (C.HaveOutput b z' x) = C.HaveOutput (go nop (Just s) a b) (z >> z') (s x)
    go z s (C.Leftover a l) b = C.Leftover (go z s a b) l
    go z s (C.NeedInput f e) b = needInput $ og z s b . either e f
    go z s a (C.Leftover b l) = C.Leftover (go z s a b) l
    go z s a (C.NeedInput f e) = needInput $ go z s a . either e f
    go z Nothing a@(C.Done _) (C.HaveOutput b z' _) = go (z >> z') Nothing a b
    og z = flip . go z
    in go nop Nothing
      (a0 C.Done)
      (b0 C.Done)

instance Monad m => Monad (ArrConduit m a) where
  ArrConduit (C.ConduitM a0) >>= f = ArrConduit $ C.ConduitM $ \rest -> let
    go (C.Done u) = rest u
    go (C.PipeM m) = C.PipeM (go <$> m)
    go (C.HaveOutput a z x) = C.unConduitM (arrConduit (f x)) (\() -> C.PipeM (go a <$ z))
    go (C.NeedInput a e) = C.NeedInput (go . a) (go . e)
    go (C.Leftover a l) = C.Leftover (go a) l
    in go (a0 C.Done)

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
  zeroArrow = ArrConduit nop

instance Monad m => ArrowPlus (ArrConduit m) where
  ArrConduit f <+> ArrConduit g = ArrConduit $ f >> g

instance Monad m => ArrowChoice (ArrConduit m) where
  ArrConduit (C.ConduitM l0) +++ ArrConduit (C.ConduitM r0) = ArrConduit $ C.ConduitM $ \rest -> let
    go (C.Done _) (C.Done _)  = rest ()
    go (C.PipeM l) r          = C.PipeM      (og r <$> l)
    go l (C.PipeM r)          = C.PipeM      (go l <$> r)
    go (C.HaveOutput l f o) r = C.HaveOutput (go l r) f (Left o)
    go l (C.HaveOutput r f o) = C.HaveOutput (go l r) f (Right o)
    go (C.Leftover l i) r     = C.Leftover   (go l r)   (Left i)
    go l (C.Leftover r i)     = C.Leftover   (go l r)   (Right i)
    go l r = C.NeedInput
      (either
        (og r . feedInput l . Right)
        (go l . feedInput r . Right))
      (\e -> go
        (feedInput l $ Left e)
        (feedInput r $ Left e))
    og = flip go
    in go
      (l0 C.Done)
      (r0 C.Done)

-- newtype ArrProduce i m a b = ArrProduce{ arrProduce :: a -> Conduit i m b }
type ArrProduce i m = Kleisli (ArrConduit m i)
type ArrSource m = ArrProduce () m

newtype ArrConsume o m a b = ArrConsume{ arrConsume :: MaybeT (ConduitM a o m) b }
  deriving (Functor, Applicative, Monad) -- MonadThrow, ...
type ArrSink = ArrConsume Void

instance Monad m => Cat.Category (ArrConsume o m) where
  id = ArrConsume $ MaybeT await
  ArrConsume (MaybeT f) . ArrConsume (MaybeT g) = ArrConsume $ MaybeT $ g >>= (.| f) . mapM_ yield

instance Monad m => Arrow (ArrConsume o m) where
  arr f = f <$> Cat.id
  ArrConsume (MaybeT (C.ConduitM l0)) *** ArrConsume (MaybeT (C.ConduitM r0)) = ArrConsume $ MaybeT $ C.ConduitM $ \rest -> let
    go (C.Done a) (C.Done b) = rest $ liftA2 (,) a b
    go (C.PipeM mx) y = C.PipeM (og y <$> mx)
    go x (C.PipeM my) = C.PipeM (go x <$> my)
    go (C.HaveOutput x f o) y = C.HaveOutput (go x y) f o
    go x (C.HaveOutput y f o) = C.HaveOutput (go x y) f o
    go (C.Leftover _ i) _ = absurd i
    go _ (C.Leftover _ i) = absurd i
    go l r = needInput $ \x -> go
      (feedInput l $ fst <$> x)
      (feedInput r $ snd <$> x)
    og = flip go
    in go
      (C.injectLeftovers $ l0 C.Done)
      (C.injectLeftovers $ r0 C.Done)

instance Monad m => ArrowZero (ArrConsume o m) where
  zeroArrow = ArrConsume $ MaybeT $ return Nothing
