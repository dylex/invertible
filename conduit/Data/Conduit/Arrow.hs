-- |Various 'Arrow' instances for "Data.Conduit"s.
-- The newtype wrappers here are somewhat similar to the types provided by old conduit versions (pre-0.4).
-- Arrows and Monads definied here function much like the list monad when dealing with multiple inputs and outputs, producing concatenation and cross-products.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Conduit.Arrow
  ( ArrConduit(..), conduitArr
  , ArrProduce, arrProduce, produceArr
  , ArrSource
  , ArrConsume(..), arrConsume, consumeArr
  , consumeArr'
  , ArrSink
  ) where

import           Control.Applicative (Alternative(..), liftA2)
import           Control.Arrow (Arrow(..), ArrowZero(..), ArrowPlus(..), ArrowChoice(..), Kleisli(..))
import qualified Control.Category as Cat
import           Control.Monad (MonadPlus(..), ap)
import           Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import           Control.Monad.IO.Class (MonadIO(..))
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

-- |A 'Conduit' represented as an 'Arrow' from input stream to output stream.
-- Composition is provided by 'fuse', with 'CL.map' as the base transformer.
-- '***' generates cross-products.
-- Monad operations function just like list monads, where 'join' is like 'concat', and inputs may be consumed by either inner or outer conduit (but not both).
newtype ArrConduit   m a b = ArrConduit
  { arrConduit :: Conduit a m b -- ^Unwrap an 'ArrConduit'
  }

-- |Wrap an 'ArrConduit'
conduitArr :: Conduit a m b -> ArrConduit m a b
conduitArr = ArrConduit
{-# INLINE conduitArr #-}

instance Monad m => Functor (ArrConduit m a) where
  fmap f (ArrConduit c) = ArrConduit $ mapOutput f c

instance Monad m => Applicative (ArrConduit m a) where
  pure = ArrConduit . yield
  (<*>) = ap

instance Monad m => Monad (ArrConduit m a) where
  ArrConduit (C.ConduitM a0) >>= f = ArrConduit $ C.ConduitM $ \rest -> let
    go (C.Done u) = rest u
    go (C.PipeM m) = C.PipeM (go <$> m)
    go (C.HaveOutput a z x) = C.unConduitM (arrConduit (f x)) (\() -> C.PipeM (go a <$ z))
    go (C.NeedInput a e) = C.NeedInput (go . a) (go . e)
    go (C.Leftover a l) = C.Leftover (go a) l
    in go (a0 C.Done)
  fail = ArrConduit . fail

instance Monad m => Alternative (ArrConduit m a) where
  empty = ArrConduit nop
  ArrConduit f <|> ArrConduit g = ArrConduit $ f >> g

instance Monad m => MonadPlus (ArrConduit m a)

instance MonadThrow m => MonadThrow (ArrConduit m a) where
  throwM = ArrConduit . throwM

instance MonadCatch m => MonadCatch (ArrConduit m a) where
  catch (ArrConduit m) f = ArrConduit $ catch m (arrConduit . f)

instance MonadIO m => MonadIO (ArrConduit m a) where
  liftIO f = ArrConduit $ liftIO f >>= yield

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

-- |A 'Kleisli' 'Arrow' from arguments to conduit outputs.
-- 'yield' provides an identity, and composition is like 'concatMap'.
type ArrProduce i m = Kleisli (ArrConduit m i)

-- |Unwrap an 'ArrProduce'
arrProduce :: ArrProduce i m a b -> a -> Conduit i m b
arrProduce (Kleisli f) = arrConduit . f
{-# INLINE arrProduce #-}

-- |Wrap an 'ArrProduce'
produceArr :: (a -> Conduit i m b) -> ArrProduce i m a b
produceArr f = Kleisli (ArrConduit . f)
{-# INLINE produceArr #-}

-- |A specialized 'ArrProduce' that consumes no inputs.
type ArrSource m = ArrProduce () m

instance Monad m => Functor (ArrProduce i m a) where
  fmap f (Kleisli a) = Kleisli $ fmap f . a

instance Monad m => Applicative (ArrProduce i m a) where
  pure a = Kleisli $ const $ pure a
  Kleisli a <*> Kleisli b = Kleisli $ \x -> a x <*> b x

instance Monad m => Monad (ArrProduce i m a) where
  Kleisli a >>= f = Kleisli $ \x -> a x >>= flip (runKleisli . f) x
  fail = Kleisli . const . fail

instance Monad m => Alternative (ArrProduce i m a) where
  empty = Kleisli $ const empty
  Kleisli f <|> Kleisli g = Kleisli $ \x -> f x <|> g x

instance Monad m => MonadPlus (ArrProduce i m a)

-- |A stream processor 'Arrow' from conduit inputs to final result, which may be empty.
-- 'await' provides an identity, and composition provides (at most) a single input to the outer conduit.
-- Monad instances are equivalent to 'ConduitM'.
newtype ArrConsume o m a b = ArrConsume (MaybeT (ConduitM a o m) b)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadThrow, MonadCatch, MonadIO)

-- |Unwrap an 'ArrConsume'
arrConsume :: ArrConsume o m a b -> ConduitM a o m (Maybe b)
arrConsume (ArrConsume (MaybeT c)) = c
{-# INLINE arrConsume #-}

-- |Wrap an 'ArrConsume'
consumeArr :: ConduitM a o m (Maybe b) -> ArrConsume o m a b
consumeArr = ArrConsume . MaybeT
{-# INLINE consumeArr #-}

-- |Wrap a successful 'ArrConsume'
consumeArr' :: ConduitM a o m b -> ArrConsume o m a b
consumeArr' = ArrConsume . MaybeT . fmap Just
{-# INLINE consumeArr' #-}

-- |A specialized 'ArrConsume' that produces no outputs.
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
