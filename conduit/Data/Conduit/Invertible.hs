{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Data.Conduit.Invertible
  ( BiConduitM(..)
  , SourceSink(..)
  , ZipSourceSink(..)
  , ProducerConsumer
  , biConsumer, biProducer
  , toProducerConsumer
  , toSourceSink

  , biFuse
  , biFuseBoth
  , biMapOutput
  , biMapInput

  , pass
  , filt
  , only
  ) where

import           Control.Applicative ((*>))
import           Control.Invertible.Monoidal
import           Control.Monad (when, unless, mfilter)
import           Data.Conduit
import qualified Data.Invertible as I

data BiConduitM o m a b = BiConduitM
  { biConduitMFwd :: ConduitM a o m b
  , biConduitMRev :: b -> Conduit o m a
  }

instance I.Functor (BiConduitM o m a) where
  fmap (f I.:<->: g) (BiConduitM c p) = BiConduitM (f <$> c) (p . g)

instance Monoidal (BiConduitM o m a) where
  unit = BiConduitM (return ()) return
  BiConduitM ca pa >*< BiConduitM cb pb = BiConduitM
    (pairADefault ca cb)
    (\(a, b) -> pa a *> pb b)

data SourceSink m a b = SourceSink
  { biSink :: Sink a m b
  , biSource :: b -> Source m a
  }

instance I.Functor (SourceSink m a) where
  fmap (f I.:<->: g) (SourceSink c p) = SourceSink (f <$> c) (p . g)

instance Monoidal (SourceSink m a) where
  unit = SourceSink (return ()) return
  SourceSink ca pa >*< SourceSink cb pb = SourceSink
    (pairADefault ca cb)
    (\(a, b) -> pa a *> pb b)

newtype ZipSourceSink m a b = ZipSourceSink{ getZipSourceSink :: SourceSink m a b }
  deriving (I.Functor)

instance Monad m => Monoidal (ZipSourceSink m a) where
  unit = ZipSourceSink $ SourceSink (return ()) return
  ZipSourceSink (SourceSink ca pa) >*< ZipSourceSink (SourceSink cb pb) = ZipSourceSink $ SourceSink
    (getZipSink $ pairADefault (ZipSink ca) (ZipSink cb))
    (\(a, b) -> getZipSource $ ZipSource (pa a) *> ZipSource (pb b))

type ProducerConsumer m a b = forall o . BiConduitM o m a b

biConsumer :: ProducerConsumer m a b -> Consumer a m b
biConsumer = biConduitMFwd

biProducer :: ProducerConsumer m a b -> b -> Producer m a
biProducer = biConduitMRev

toProducerConsumer :: Monad m => SourceSink m a b -> ProducerConsumer m a b
toProducerConsumer (SourceSink c p) = BiConduitM (toConsumer c) (toProducer . p)

toSourceSink :: ProducerConsumer m a b -> SourceSink m a b
toSourceSink c = SourceSink (biConduitMFwd c) (biConduitMRev c)

biFuse :: Monad m => BiConduitM b m a () -> BiConduitM c m b r -> BiConduitM c m a r
biFuse (BiConduitM fa ra) (BiConduitM fb rb) =
  BiConduitM (fuse fa fb) (\x -> fuse (rb x) (ra ()))

biFuseBoth :: Monad m => BiConduitM b m a r1 -> BiConduitM c m b r2 -> BiConduitM c m a (r1, r2)
biFuseBoth (BiConduitM fa ra) (BiConduitM fb rb) =
  BiConduitM (fuseBoth fa fb) (\(x, y) -> fuse (rb y) (ra x))

biMapOutput :: Monad m => (o1 I.<-> o2) -> BiConduitM o1 m i r -> BiConduitM o2 m i r
biMapOutput (f I.:<->: g) (BiConduitM c p) =
  BiConduitM (mapOutput f c) (mapInput g (Just . f) . p)

biMapInput :: Monad m => (i1 I.<-> i2) -> BiConduitM o m i2 r -> BiConduitM o m i1 r
biMapInput (f I.:<->: g) (BiConduitM c p) =
  BiConduitM (mapInput f (Just . g) c) (mapOutput g . p)


pass :: Monad m => BiConduitM o m a (Maybe a)
pass = BiConduitM await (mapM_ yield)

filt :: Monad m => (a -> Bool) -> BiConduitM o m a (Maybe a)
filt f = BiConduitM
  (maybe (return Nothing) (\x -> if f x then return (Just x) else Nothing <$ leftover x) =<< await)
  (mapM_ yield . mfilter f)

only :: (Eq a, Monad m) => a -> BiConduitM o m a Bool
only y = BiConduitM
  (maybe (return False) (\x -> True <$ unless (y == x) (leftover x)) =<< await)
  (\b -> when b $ yield y)
