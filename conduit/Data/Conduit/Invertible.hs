-- |Reversible conduit pairs that can be combined using "Control.Invertible.Monoidal", useful for parser/generators.
-- Note that these focus on manipulating the generated result (@r@ in "Data.Conduit"), not the streams themselves.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Data.Conduit.Invertible
  ( BiConduitM(..)
  , SourceSink
  , biSink, biSource
  , ZipSourceSink(..)
  , ProducerConsumer
  , biConsumer, biProducer
  , toProducerConsumer

  , biFuse
  , biFuseBoth
  , biMapStream

  , pass
  , predicate
  , exactly
  , partial
  ) where

import           Control.Applicative (liftA2, (*>))
import           Control.Invertible.Monoidal
import           Control.Monad (void, when, guard)
import           Data.Conduit
import qualified Data.Invertible as I
import           Data.Maybe (isNothing)
import           Data.Void (Void, absurd)

-- |Combine two conduits that are inverses of each other: one processes a stream to produce a final value, and the other takes that value to generate a stream.
-- The forward processor may fail (using 'Nothing') in order to admit choice, but it must not consume any input if it does so.
-- 'BiConduitM' may be combined using 'I.Functor', 'Monoidal', and 'MonoidalAlt' instances.
data BiConduitM i o m a b = BiConduitM -- :<-|->:
  { biConduitMFwd :: ConduitM a o m (Maybe b) -- ^a forward processor that processes input @a@ into a final result @b@ (possibly producing output @o@)
  , biConduitMRev :: b -> Conduit i m a -- ^and a reverse generator that, given a @b@, produces output @o@ (possibly taking input @i@)
  }

instance I.Functor (BiConduitM i o m a) where
  fmap (f I.:<->: g) (BiConduitM c p) = BiConduitM (fmap f <$> c) (p . g)

instance Monoidal (BiConduitM i o m a) where
  unit = BiConduitM (pure $ Just ()) return
  BiConduitM ca pa >*< ~(BiConduitM cb pb) = BiConduitM
    (maybe (return Nothing) (\a -> fmap (a, ) <$> cb) =<< ca)
    (\(a, b) -> pa a *> pb b)

instance MonoidalAlt (BiConduitM i o m a) where
  zero = BiConduitM (pure Nothing) (return . absurd)
  BiConduitM ca pa >|< ~(BiConduitM cb pb) = BiConduitM
    (maybe (fmap Right <$> cb) (return . Just . Left) =<< ca)
    (either pa pb)

-- |A special case of 'BiConduitM' where the conduits are a 'Source' and 'Sink'.
type SourceSink = BiConduitM () Void

-- |Specialization of 'biConduitMFwd'
biSink :: SourceSink m a b -> Sink a m (Maybe b)
biSink = biConduitMFwd

-- |Specialization of 'biConduitMRev'
biSource :: SourceSink m a b -> b -> Source m a
biSource = biConduitMRev

-- |Alternative 'Monoidal' instance for 'SourceSink' that uses 'ZipSource' and 'ZipSink' when combining.
newtype ZipSourceSink m a b = ZipSourceSink{ getZipSourceSink :: SourceSink m a b }
  deriving (I.Functor)

instance Monad m => Monoidal (ZipSourceSink m a) where
  unit = ZipSourceSink unit
  ZipSourceSink (BiConduitM ca pa) >*< ZipSourceSink (BiConduitM cb pb) = ZipSourceSink $ BiConduitM
    (getZipSink $ liftA2 pairADefault (ZipSink ca) (ZipSink cb))
    (\(a, b) -> getZipSource $ ZipSource (pa a) *> ZipSource (pb b))

-- |A special case of 'BiConduitM' where the conduits are a 'Producer' and 'Consumer'.
type ProducerConsumer m a b = forall i o . BiConduitM i o m a b

-- |Specialization of 'biConduitMFwd'
biConsumer :: ProducerConsumer m a b -> Consumer a m (Maybe b)
biConsumer = biConduitMFwd

-- |Specialization of 'biConduitMRev'
biProducer :: ProducerConsumer m a b -> b -> Producer m a
biProducer = biConduitMRev

-- |Combine 'toConsumer' and 'toProducer'.
toProducerConsumer :: Monad m => SourceSink m a b -> ProducerConsumer m a b
toProducerConsumer (BiConduitM c p) = BiConduitM (toConsumer c) (toProducer . p)

-- |'fuse' two 'BiConduitM's.  Ignores a failure of the first conduit.
biFuse :: Monad m => BiConduitM b b m a () -> BiConduitM ci co m b r -> BiConduitM ci co m a r
biFuse (BiConduitM fa ra) (BiConduitM fb rb) =
  BiConduitM (fuse (void fa) fb) (\x -> fuse (rb x) (ra ()))

-- |'fuseBoth' two 'BiConduitM's.  Fails if either conduit fails.
biFuseBoth :: Monad m => BiConduitM b b m a r1 -> BiConduitM c c m b r2 -> BiConduitM c c m a (r1, r2)
biFuseBoth (BiConduitM fa ra) (BiConduitM fb rb) =
  BiConduitM (uncurry pairADefault <$> fuseBoth fa fb) (\(x, y) -> fuse (rb y) (ra x))

-- |Apply 'mapInput' on forward conduit and 'mapOutput' on reverse, changing the type of the stream.
biMapStream :: Monad m => (a I.<-> b) -> BiConduitM i o m b r -> BiConduitM i o m a r
biMapStream (f I.:<->: g) (BiConduitM c p) =
  BiConduitM (mapInput f (Just . g) c) (mapOutput g . p)


foldMapM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
foldMapM = maybe (return Nothing)

-- |Take/give a single value on the stream (the identity 'BiConduitM')
pass :: Monad m => BiConduitM i o m a a
pass = BiConduitM await yield

-- |Only take/give values matching the given predecate; fail or give nothing otherwise
predicate :: Monad m => (a -> Bool) -> BiConduitM i o m a a
predicate f = BiConduitM
  (foldMapM (\x -> if f x then return $ Just x else Nothing <$ leftover x) =<< await)
  (\x -> when (f x) $ yield x)

-- |Only take/give a single value, failing on anything else.
exactly :: (Eq a, Monad m) => a -> BiConduitM i o m a ()
exactly y = partial (guard . (y ==)) (const y)
-- exactly y = constI y $ predicate (y ==)

-- |Only take values that map to 'Just', and give the result of the reverse mapping.
-- This is like the partial version of @('>$<' 'pass')@.
partial :: Monad m => (a -> Maybe b) -> (b -> a) -> BiConduitM i o m a b
partial f g = BiConduitM
  (foldMapM (\x -> let y = f x in y <$ when (isNothing y) (leftover x)) =<< await)
  (yield . g)
