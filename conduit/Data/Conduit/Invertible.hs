-- |Reversible conduit pairs that can be combined using "Control.Invertible.Monoidal", useful for parser/generators.
-- Note that these focus on manipulating the generated result (@r@ in "Data.Conduit"), not the streams themselves.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Data.Conduit.Invertible
  ( BiConduit

    -- * Parser/Generators
  , ConsumeProduce(..)
  , biConsumeProduce
    -- ** Failure recovery
  , (>*?<)
  , tryConsumeProduce
    -- ** Specializations
  , SourceSink
  , biSink, biSource
  , ZipSourceSink(..)
  , ProducerConsumer
  , biConsumer, biProducer
  , toProducerConsumer

    -- ** Operations
  , biFuse
  , biFuseBoth
  , biMapStream

  , pass
  , predicate
  , exactly
  , partial
  ) where

import           Control.Applicative (liftA2)
import           Control.Arrow (Kleisli(..))
import qualified Control.Category as Cat
import           Control.Invertible.Monoidal
import           Control.Monad (void, when, guard)
import           Data.Conduit
import qualified Data.Invertible as I
import           Data.Maybe (isNothing)
import           Data.Void (Void, absurd)

import           Data.Conduit.Arrow

-- |A simple invertible conduit stream.
type BiConduit m = I.Bijection (ArrConduit m)

-- |Combine two conduits that are inverses of each other: one processes a stream down to a final value, and the other takes that value to generate a stream.
-- The forward processor may fail (using 'Nothing') in order to admit choice, but if they consume any input, choice may not work as expected.  You can use '>*?<' and 'tryConsumeProduce' to allow rolling back consumed input.
-- 'ConsumeProduce' may be combined using 'I.Functor', 'Monoidal', and 'MonoidalAlt' instances.
data ConsumeProduce i o m a b = ConsumeProduce
  { biConsume :: ArrConsume o m a b -- ^a forward processor that processes input @a@ into a final result @b@ (possibly producing output @o@)
  , biProduce :: ArrProduce i m b a -- ^and a reverse generator that, given a @b@, produces output @o@ (possibly taking input @i@)
  }

-- |Construct a 'ConsumeProduce' from 'ConduitM's.
biConsumeProduce :: ConduitM a o m (Maybe b) -> (b -> Conduit i m a) -> ConsumeProduce i o m a b
biConsumeProduce c p = ConsumeProduce (consumeArr c) (produceArr p)
{-# INLINE biConsumeProduce #-}

instance Monad m => Cat.Category (ConsumeProduce i o m) where
  id = ConsumeProduce Cat.id Cat.id
  ConsumeProduce cf pf . ConsumeProduce cg pg = ConsumeProduce (cf Cat.. cg) (pg Cat.. pf)

instance I.Functor (ConsumeProduce i o m a) where
  fmap (f I.:<->: g) (ConsumeProduce c p) = biConsumeProduce
    (fmap f <$> arrConsume c)
    (arrProduce p . g)

instance Monoidal (ConsumeProduce i o m a) where
  unit = biConsumeProduce (pure $ Just ()) return
  ConsumeProduce ca pa >*< ~(ConsumeProduce cb pb) = ConsumeProduce
    (pairADefault ca cb)
    (produceArr $ \(a, b) -> arrProduce pa a *> arrProduce pb b)

-- |Like '>*<' but uses '>>?=' on the consumer, so that if the consumer fails, any input consumed by the left action (but not the right) will be rolled back as leftovers.
(>*?<) :: Functor m => ConsumeProduce i o m a b -> ConsumeProduce i o m a c -> ConsumeProduce i o m a (b, c)
ConsumeProduce ca pa >*?< ~(ConsumeProduce cb pb) = ConsumeProduce
  (ca >>?= \a -> (,) a <$> cb)
  (produceArr $ \(a, b) -> arrProduce pa a *> arrProduce pb b)

infixr 4 >*?<

-- |Apply 'tryConsume' to the consumer.
tryConsumeProduce :: Functor m => ConsumeProduce i o m a b -> ConsumeProduce i o m a b
tryConsumeProduce (ConsumeProduce c p) = ConsumeProduce (tryConsume c) p

instance MonoidalAlt (ConsumeProduce i o m a) where
  zero = biConsumeProduce (pure Nothing) (return . absurd)
  ConsumeProduce ca pa >|< ~(ConsumeProduce cb pb) = biConsumeProduce
    (arrConsume ca >>= maybe (fmap Right <$> arrConsume cb) (return . Just . Left))
    (either (arrProduce pa) (arrProduce pb))

-- |A special case of 'ConsumeProduce' where the conduits are a 'Source' and 'Sink'.
type SourceSink = ConsumeProduce () Void

-- |Specialization of 'biConsume'
biSink :: SourceSink m a b -> Sink a m (Maybe b)
biSink = arrConsume . biConsume

-- |Specialization of 'biProduce'
biSource :: SourceSink m a b -> b -> Source m a
biSource = arrProduce . biProduce

-- |Alternative 'Monoidal' instance for 'SourceSink' that uses 'ZipSource' and 'ZipSink' when combining.
newtype ZipSourceSink m a b = ZipSourceSink{ getZipSourceSink :: SourceSink m a b }
  deriving (I.Functor)

instance Monad m => Monoidal (ZipSourceSink m a) where
  unit = ZipSourceSink unit
  ZipSourceSink (ConsumeProduce ca pa) >*< ZipSourceSink (ConsumeProduce cb pb) = ZipSourceSink $ biConsumeProduce
    (getZipSink $ liftA2 pairADefault (ZipSink $ arrConsume ca) (ZipSink $ arrConsume cb))
    (\(a, b) -> getZipSource $ ZipSource (arrProduce pa a) *> ZipSource (arrProduce pb b))

-- |A special case of 'ConsumeProduce' where the conduits are a 'Producer' and 'Consumer'.
type ProducerConsumer m a b = forall i o . ConsumeProduce i o m a b

-- |Specialization of 'biConsume'
biConsumer :: ProducerConsumer m a b -> Consumer a m (Maybe b)
biConsumer (ConsumeProduce c _) = arrConsume c

-- |Specialization of 'biProduce'
biProducer :: ProducerConsumer m a b -> b -> Producer m a
biProducer (ConsumeProduce _ (Kleisli p)) = arrConduit . p

-- |Combine 'toConsumer' and 'toProducer'.
toProducerConsumer :: Monad m => SourceSink m a b -> ProducerConsumer m a b
toProducerConsumer (ConsumeProduce c p) =
  biConsumeProduce (toConsumer $ arrConsume c) (toProducer . arrProduce p)

-- |'fuse' two 'ConsumeProduce's.  Ignores a failure of the first conduit.
biFuse :: Monad m => ConsumeProduce b b m a () -> ConsumeProduce ci co m b r -> ConsumeProduce ci co m a r
biFuse (ConsumeProduce fa ra) (ConsumeProduce fb rb) = biConsumeProduce
  (fuse (void $ arrConsume fa) $ arrConsume fb)
  (\x -> fuse (arrProduce rb x) (arrProduce ra ()))

-- |'fuseBoth' two 'ConsumeProduce's.  Fails if either conduit fails.
biFuseBoth :: Monad m => ConsumeProduce b b m a r1 -> ConsumeProduce c c m b r2 -> ConsumeProduce c c m a (r1, r2)
biFuseBoth (ConsumeProduce fa ra) (ConsumeProduce fb rb) = biConsumeProduce
  (uncurry pairADefault <$> arrConsume fa `fuseBoth` arrConsume fb)
  (\(x, y) -> fuse (arrProduce rb y) (arrProduce ra x))

-- |Apply 'mapInput' on forward conduit and 'mapOutput' on reverse, changing the type of the stream.
biMapStream :: Monad m => (a I.<-> b) -> ConsumeProduce i o m b r -> ConsumeProduce i o m a r
biMapStream (f I.:<->: g) (ConsumeProduce c p) = biConsumeProduce
  (mapInput f (Just . g) $ arrConsume c)
  (mapOutput g . arrProduce p)


foldMapM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
foldMapM = maybe (return Nothing)

-- |Take/give a single value on the stream.
-- This is also 'Cat.id'.
pass :: Monad m => ConsumeProduce i o m a a
pass = biConsumeProduce await yield

-- |Only take/give values matching the given predecate; fail and give/take nothing otherwise.
predicate :: Monad m => (a -> Bool) -> ConsumeProduce i o m a a
predicate f = biConsumeProduce
  (foldMapM (\x -> if f x then return $ Just x else Nothing <$ leftover x) =<< await)
  (\x -> when (f x) $ yield x)

-- |Only take/give a single value, failing and taking nothing on anything else.
exactly :: (Eq a, Monad m) => a -> ConsumeProduce i o m a ()
exactly y = partial (guard . (y ==)) (const y)
-- exactly y = constI y $ predicate (y ==)

-- |Only take values that map to 'Just', and give the result of the reverse mapping.
-- This is like the partial version of @('>$<' 'pass')@.
partial :: Monad m => (a -> Maybe b) -> (b -> a) -> ConsumeProduce i o m a b
partial f g = biConsumeProduce
  (foldMapM (\x -> let y = f x in y <$ when (isNothing y) (leftover x)) =<< await)
  (yield . g)
