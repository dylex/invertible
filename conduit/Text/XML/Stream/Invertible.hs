module Text.XML.Stream.Invertible
  ( Streamer
  , content
  , contentMaybe

  , AttrStreamer(..)
  , optionalAttr
  , requiredAttr
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

type Streamer m a = SourceSink m X.Event a

content :: MonadThrow m => Streamer m T.Text
content = SourceSink P.content R.content

contentMaybe :: MonadThrow m => Streamer m (Maybe T.Text)
contentMaybe = SourceSink P.contentMaybe (mapM_ R.content)

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

optionalAttr :: X.Name -> AttrStreamer (Maybe T.Text)
optionalAttr n = AttrStreamer (P.attr n) (R.optionalAttr n)

requiredAttr :: X.Name -> AttrStreamer T.Text
requiredAttr n = AttrStreamer (P.requireAttr n) (R.attr n)

ignoreAttrs :: AttrStreamer ()
ignoreAttrs = AttrStreamer P.ignoreAttrs mempty
