module Data.Conduit.Flush
  ( flushWhen
  , flushAfter
  , flushEach
  , unFlush
  ) where

import           Data.Conduit
import           Data.Foldable (fold)
import           Data.Monoid ((<>), Sum(..))

-- |Accumulate conduit values in a monoid and flush whenever the condition is true.
-- @flushWhen monoid condition@ will flush first if @condition 'mempty'@ holds, but then only check condition after each value.
-- It will always append a flush at the end of a stream, regardless of condition.
flushWhen :: (Monoid b, Monad m) => (a -> b) -> (b -> Bool) -> Conduit a m (Flush a)
flushWhen w f = run (Just mempty) where
  run (Just x) 
    | f x = yield Flush >> run Nothing
    | otherwise = maybe (yield Flush) (next $ mappend x) =<< await
  run Nothing = mapM_ (next id) =<< await
  next m a = yield (Chunk a) >> run (Just $ m $ w a)

-- |Add up conduit values, and flush once the sum passes a threshold.
-- Applies 'flushWhen' over the 'Sum' monoid.
flushAfter :: (Num b, Ord b, Monad m) => (a -> b) -> b -> Conduit a m (Flush a)
flushAfter w t = flushWhen (Sum . w) ((t <=) . getSum)

-- |Insert a flush every @n@ values (i.e., 1 will flush after each chunk; values @<= 0@ are like 1 but will flush first).
-- Equivalent to @'flushAfter' (const 1)@.
flushEach :: Monad m => Int -> Conduit a m (Flush a)
flushEach = flushAfter (const 1)

-- |Convert a monoid flush stream into a reduced stream, where concatenated values are produced for each 'Flush' (and possibly end of stream).
unFlush :: (Monoid a, Monad m) => Conduit (Flush a) m a
unFlush = run Nothing where
  run x = maybe (mapM_ yield x) (next x) =<< await
  next x Flush = yield (fold x) >> run Nothing
  next x (Chunk y) = run $ Just $ maybe y (<> y) x
