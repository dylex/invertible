-- |Combine "Text.XML.Stream.Parser" and "Text.XML.Stream.Render" into a single, invertible stream using "Data.Conduit.Invertible".
--
-- These functions match the names in "Text.XML.Stream.Parser" as closely as possible, even when they aren't necessariy consistent.
{-# LANGUAGE TupleSections #-}
module Text.XML.Stream.Invertible
  ( module Control.Invertible.Monoidal
    -- * XML stream processing
  , Streamer
  , streamerParser
  , streamerRender
    -- ** Error checking
  , force
  , convert

    -- * Event processing
    -- ** Text content
  , content
  , requireContent
  , stringContent
  , readShowContent
    -- ** Tags
    -- *** Tags with content
  , tag
  , tagPredicate
  , tagName
  , tagNoAttr
  , tagIgnoreAttrs
    -- *** Tags with no content
  , emptyTag
  , emptyTagName
  , ignoreTag
  , ignoreTagName
  , ignoreAllTags
    -- *** Tags with ignored content
  , ignoreTree
  , ignoreTreeName
  , ignoreAllTrees
  , ignoreNodes

    -- * Attribute parsing
  , AttrStreamer(..)
  , attr
  , ignoreAttrs

    -- * XML processing
  , ignoreWhitespace
  , passElement
  , passNode
  , passNodes
  ) where

import           Control.Applicative (empty)
import           Control.Invertible.Monoidal
import           Control.Monad (when, void)
import           Control.Monad.Catch (MonadThrow)
import           Data.Char (isSpace)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Invertible as I
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Void (absurd)
import qualified Data.XML.Types as X
import           Text.Read (readEither)
import qualified Text.XML.Stream.Parse as P
import qualified Text.XML.Stream.Render as R
import qualified Text.XML.Unresolved as U

import           Data.Conduit.Invertible

-- |Combine a parser and a generator.
-- Note that all 'BiConduitM' parsers may fail so that they can be combined using 'MonoidalAlt' (e.g., made optional with 'optionalI').
-- However, XML parsers can also throw errors using 'MonadThrow', and these errors are not recoverable.
type Streamer m = SourceSink m X.Event

-- |Extract a parser from a 'Streamer', throwing the given error message if it fails.
streamerParser :: MonadThrow m => String -> Streamer m a -> C.Sink X.Event m a
streamerParser e = P.force e . biSink

streamerRender :: Streamer m a -> a -> C.Source m X.Event
streamerRender = biSource

justIf :: (a -> Bool) -> a -> Maybe a
justIf p x
  | p x = Just x
  | otherwise = Nothing

-- |Apply 'P.force', forcing a 'Streamer' to succeed and throwing an error if not.
-- (This is not encoded in the type because 'BiConduitM's can always fail, although 'force' never does.)
force :: MonadThrow m => String -> Streamer m a -> Streamer m a
force e (BiConduitM p r) = BiConduitM (Just <$> P.force e p) r

-- |Attempt to convert a value, throwing an (unrecoverable) error message on 'Left'.
-- This is like a partial version of '>$<'.
convert :: MonadThrow m
  => (a -> Either String b) -- ^Parsing convert function, either an error message or successful result
  -> (b -> a) -- ^Inverse render function
  -> Streamer m a -- ^Streamer to convert
  -> Streamer m b
convert f g (BiConduitM p r) = BiConduitM
  (mapM (either (`P.force` return Nothing) return . f) =<< p)
  (r . g)

-- |Take/give a next piece of text content, empty if there is none (never fails: 'P.content' and 'R.content')
content :: MonadThrow m => Streamer m T.Text
content = BiConduitM (Just <$> P.content) R.content

-- |Require a non-empty piece of text content, failing if there is none ('P.contentMaybe' and 'R.content')
requireContent :: MonadThrow m => Streamer m T.Text
requireContent = BiConduitM P.contentMaybe R.content

-- |Process 'content' as a 'String'.
stringContent :: MonadThrow m => Streamer m String
stringContent = (T.unpack :<->: T.pack) >$< content

-- |Use primitive 'Read' and 'Show' instances to process 'content'.
readShowContent :: (Read a, Show a, MonadThrow m) => Streamer m a
readShowContent = convert readEither show stringContent

-- |Require a tag, providing handlers for attributes and body.
-- Both of the handlers are essentially 'force'd, so that this will throw an error if either fail.
-- The whole action fails iff there is no next tag (i.e., content or end).
tag :: MonadThrow m
  => AttrStreamer a -- ^attribute handler
  -> Streamer m b -- ^children handler
  -> Streamer m (X.Name, a, b)
tag = tagPredicate (const True)

-- |Require a tag with a name that matches the given predicate, providing handlers for attributes and body.
-- Both of the handlers are essentially 'force'd, so that this will throw an error if either fail.
-- The whole action fails iff the next tag's name does not match.
tagPredicate :: MonadThrow m
  => (X.Name -> Bool) -- ^a predicate to match the (input or argument) tag name against
  -> AttrStreamer a -- ^attribute handler
  -> Streamer m b -- ^children handler
  -> Streamer m (X.Name, a, b)
tagPredicate np (AttrStreamer ap ar) (BiConduitM bp br) = BiConduitM
  (P.tag (justIf np) ((<$> ap) . (,)) $ \(n,a) -> (,,) n a <$> P.force ("failed to parse body of tag " ++ show n) bp)
  (\(n, a, b) -> when (np n) $ R.tag n (ar a) (br b))

-- |Require a tag with the given name, providing handlers for attributes and body.
-- Both of the handlers are essentially 'force'd, so that this will throw an error if either fail.
-- The whole action fails iff the next tag's name does not match.
tagName :: MonadThrow m
  => X.Name -- ^exact tag name to match
  -> AttrStreamer a -- ^attribute handler
  -> Streamer m b -- ^children handler
  -> Streamer m (a, b)
tagName n (AttrStreamer ap ar) (BiConduitM bp br) = BiConduitM
  (P.tagName n ap $ (<$> P.force ("failed to parse body of tag " ++ show n) bp) . (,))
  (\(a, b) -> R.tag n (ar a) (br b))

-- |Require a tag with the given name, generating an error for any attributes, providing a handler for the body.
-- The body handler is 'force'd, so that this will throw an error it fails.
-- The whole action fails iff the next tag's name does not match.
tagNoAttr :: MonadThrow m
  => X.Name -- ^exact tag name to match
  -> Streamer m b -- ^children handler
  -> Streamer m b
tagNoAttr n b = I.snd >$< tagName n unit b

-- |Require a tag with the given name, ignoring any attributes, providing a handler for the body.
-- The body handler is 'force'd, so that this will throw an error it fails.
-- The whole action fails iff the next tag's name does not match.
tagIgnoreAttrs :: MonadThrow m
  => X.Name -- ^exact tag name to match
  -> Streamer m b -- ^children handler
  -> Streamer m b
tagIgnoreAttrs n b = I.snd >$< tagName n ignoreAttrs b

-- tagPredicateIgnoreAttrs doesn't seem all that useful...

emptyTagR :: Monad m => X.Name -> C.Source m X.Event
emptyTagR n = R.tag n mempty (return ())

-- |Require a tag with a name that matches the given predicate, producing an error for any attributes or children.
-- The whole action fails iff the next tag's name does not match.
emptyTag :: MonadThrow m => (X.Name -> Bool) -> Streamer m X.Name
emptyTag np = BiConduitM
  (P.tag (justIf np) return return)
  (\n -> when (np n) $ emptyTagR n)

-- |Require a tag with the given name, producing an error for any attributes or children.
-- The whole action fails iff the next tag's name does not match.
emptyTagName :: MonadThrow m => X.Name -> Streamer m ()
emptyTagName n = BiConduitM
  (P.tagName n (return ()) return)
  (\() -> emptyTagR n)

-- |Require a tag with a name that matches the given predicate, ignoring any attributes and producing an error for any children.
-- The whole action fails iff the next tag's name does not match.
ignoreTag :: MonadThrow m => (X.Name -> Bool) -> Streamer m X.Name
ignoreTag np = BiConduitM
  (P.tag (justIf np) (<$ P.ignoreAttrs) return)
  (\n -> when (np n) $ emptyTagR n)

-- |Require a tag with the given name, ignoring any attributes and producing an error for any children.
-- The whole action fails iff the next tag's name does not match.
ignoreTagName :: MonadThrow m => X.Name -> Streamer m ()
ignoreTagName n = BiConduitM
  (P.ignoreTagName n)
  (\() -> emptyTagR n)

-- |Require a (single) tag, ignoring any attributes and producing an error for any children.
-- The whole action fails iff there is no next tag (i.e., content or end).
ignoreAllTags :: MonadThrow m => Streamer m X.Name
ignoreAllTags = ignoreTag (const True)

-- |Require a tag with a name that matches the given predicate, ignoring any attributes and children recursively.
-- The whole action fails iff the next tag's name does not match.
ignoreTree :: MonadThrow m => (X.Name -> Bool) -> Streamer m X.Name 
ignoreTree np = BiConduitM
  (P.tag (justIf np) (<$ P.ignoreAttrs) (<$ ignoreNodesP))
  (\n -> when (np n) $ emptyTagR n)

-- |Require a tag with the given name, ignoring any attributes and children recursively.
-- The whole action fails iff the next tag's name does not match.
ignoreTreeName :: MonadThrow m => X.Name -> Streamer m ()
ignoreTreeName n = BiConduitM
  (P.ignoreTreeName n)
  (\() -> emptyTagR n)

-- |Require a (single) tag, ignoring any attributes and children recursively.
-- The whole action fails iff there is no next tag (i.e., content or end).
ignoreAllTrees :: MonadThrow m => Streamer m X.Name 
ignoreAllTrees = ignoreTree (const True)

-- this seems inefficient, but it's what Parse does.
ignoreNodesP :: MonadThrow m => C.Sink X.Event m ()
ignoreNodesP = void $ P.many' $ return Nothing

-- |Ignore any remaining tags and content, producing nothing.
ignoreNodes :: MonadThrow m => Streamer m ()
ignoreNodes = BiConduitM (Just <$> ignoreNodesP) return


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


ignoreWhitespaceP :: Monad m => C.Sink X.Event m ()
ignoreWhitespaceP =
  mapM_ (\x -> if isWhitespace x then ignoreWhitespaceP else C.leftover x) =<< C.await
  where
  isWhitespace X.EventBeginDocument               = True
  isWhitespace X.EventEndDocument                 = True
  isWhitespace X.EventBeginDoctype{}              = True
  isWhitespace X.EventEndDoctype                  = True
  isWhitespace X.EventInstruction{}               = True
  isWhitespace (X.EventContent (X.ContentText t)) = T.all isSpace t
  isWhitespace X.EventComment{}                   = True
  isWhitespace _                                  = False

-- |Ignore any whitespace.
ignoreWhitespace :: Monad m => Streamer m ()
ignoreWhitespace = BiConduitM (Just <$> ignoreWhitespaceP) return

elementP :: MonadThrow m => C.Sink X.Event m (Maybe X.Element)
elementP = C.mapInput (Nothing, ) (Just . snd) U.elementFromEvents

elementR :: Monad m => X.Element -> C.Source m X.Event
-- elementR = CL.sourceList . U.elementToEvents)
elementR (X.Element n a b) = do
  C.yield $ X.EventBeginElement n a
  mapM_ nodeR b
  C.yield $ X.EventEndElement n

-- |Pass an element, failing if the next item is not an element, ignoring any leading whitespace (like 'tag').
passElement :: MonadThrow m => Streamer m X.Element
passElement = BiConduitM
  (ignoreWhitespaceP >> elementP)
  (CL.sourceList . U.elementToEvents)

nodeP :: MonadThrow m => C.Sink X.Event m (Maybe X.Node)
nodeP = do
  x <- C.await
  case x of
    Just e@X.EventBeginElement{} -> C.leftover e >> fmap X.NodeElement <$> elementP
    Just (X.EventInstruction i) -> return $ Just $ X.NodeInstruction i
    Just (X.EventContent c) -> return $ Just $ X.NodeContent c
    Just (X.EventComment t) -> return $ Just $ X.NodeComment t
    Just (X.EventCDATA t) -> return $ Just $ X.NodeContent $ X.ContentText t
    Just e -> Nothing <$ C.leftover e
    Nothing -> return Nothing

nodeR :: Monad m => X.Node -> C.Source m X.Event
nodeR (X.NodeElement e)     = elementR e
nodeR (X.NodeInstruction i) = C.yield $ X.EventInstruction i
nodeR (X.NodeContent c)     = C.yield $ X.EventContent c
nodeR (X.NodeComment t)     = C.yield $ X.EventComment t

-- |Pass any single node.
-- Unlike all the other parsers, this does not ignore whitespace, but returns it as-is.
passNode :: MonadThrow m => Streamer m X.Node
passNode = BiConduitM nodeP nodeR

-- |Pass a list of nodes (i.e., arbitrary content).
-- Equivalent to @'manyI' 'passNode'@.
-- This never fails (but it can throw errors).
passNodes :: MonadThrow m => Streamer m [X.Node]
passNodes = BiConduitM (Just <$> P.many (C.toConsumer nodeP)) (mapM_ nodeR)
