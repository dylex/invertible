-- |Combine "Text.XML.Stream.Parser" and "Text.XML.Stream.Generator" into a single, invertible stream using "Data.Conduit.Invertible".
module Text.XML.Stream.Invertible
  ( module Control.Invertible.Monoidal
  , Streamer
  , force
  , content
  , requireContent

  , AttrStreamer(..)
  , attr
  , ignoreAttrs
  ) where

import           Control.Applicative (empty)
import           Control.Invertible.Monoidal
import           Control.Monad.Catch (MonadThrow)
import qualified Data.Invertible as I
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Void (absurd)
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as P
import qualified Text.XML.Stream.Render as R

import           Data.Conduit.Invertible

-- |Combine a parser and a generator.
-- Note that all parsers may fail, so that they can be combined using 'MonoidalAlt' (and use 'optionalI' to test).
type Streamer m = SourceSink m X.Event

-- |Apply 'P.force', forcing a 'Streamer' to succeed and throwing an error if not.
-- (This is not encoded in the type because 'BiConduitM's can always fail.)
force :: MonadThrow m => String -> Streamer m a -> Streamer m a
force e (BiConduitM p r) = BiConduitM (Just <$> P.force e p) r

-- |Take/give a next piece of text content, empty if there is none (never fails: 'P.content' and 'R.content')
content :: MonadThrow m => Streamer m T.Text
content = BiConduitM (Just <$> P.content) R.content

-- |Require a non-empty piece of text content, failing if there is none ('P.contentMaybe' and 'R.content')
requireContent :: MonadThrow m => Streamer m T.Text
requireContent = BiConduitM P.contentMaybe R.content

-- tag :: MonadThrow m => Name -> AttrStreamer a -> Streamer m b -> Streamer m (a, b)

-- |Combine 'P.AttrParser' and generated 'R.Attributes'.
-- Like 'R.AttrParser', parsers may fail and be handled using 'MonoidalAlt' (or made optional using 'optionalI').
data AttrStreamer a = AttrStreamer
  { attrParser :: P.AttrParser a
  , attrRender :: a -> R.Attributes
  }

instance I.Functor AttrStreamer where
  fmap (f I.:<->: g) (AttrStreamer p r) = AttrStreamer (f <$> p) (r . g)

instance Monoidal AttrStreamer where
  unit = AttrStreamer (pure ()) (\() -> mempty)
  AttrStreamer pa ra >*< AttrStreamer pb rb = AttrStreamer
    (pairADefault pa pb) (\(a, b) -> ra a <> rb b)

instance MonoidalAlt AttrStreamer where
  zero = AttrStreamer empty absurd
  AttrStreamer pa ra >|< AttrStreamer pb rb = AttrStreamer
    (eitherADefault pa pb) (either ra rb)

-- |'P.requireAttr' and 'R.attr'.
attr :: X.Name -> AttrStreamer T.Text
attr n = AttrStreamer (P.requireAttr n) (R.attr n)

-- |'P.ignoreAttrs', generating no attributes.
ignoreAttrs :: AttrStreamer ()
ignoreAttrs = AttrStreamer P.ignoreAttrs mempty
