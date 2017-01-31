{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.XML.HXT.Arrow.Pickle.Xml.Invertible
  ( module Text.XML.HXT.Arrow.Pickle.Xml
  , Inv.biCase
  , Inv.Bijection(..)
  , module Control.Invertible.Monoidal
  , xpWhitespace
  , xpTrim
  , xpAnyCont
  , xpAnyAttrs
  , xpAny
  , xpAnyElem
  ) where

import           Control.Invertible.Monoidal
import           Control.Monad.State.Class (modify, state)
import           Data.Char.Properties.XMLCharProps (isXmlSpaceChar)
import qualified Data.Invertible as Inv
import           Data.List (partition)
import           Data.Void (absurd)
import           Text.XML.HXT.Arrow.Pickle.Schema (Schema(Any), scEmpty, scSeq, scAlt, scNull)
import           Text.XML.HXT.Arrow.Pickle.Xml
import qualified Text.XML.HXT.Core as HXT
import qualified Text.XML.HXT.DOM.XmlNode as XN

instance Inv.Functor PU where
  fmap (f Inv.:<->: g) p = PU -- xpWrap
    { appPickle = appPickle p . g
    , appUnPickle = fmap f $ appUnPickle p
    , theSchema = theSchema p
    }

instance Monoidal PU where
  unit = xpUnit
  p >*< q = PU -- xpPair
    { appPickle = \(a, b) -> appPickle p a . appPickle q b
    , appUnPickle = do
        a <- appUnPickle p 
        b <- appUnPickle q
        return (a, b)
    , theSchema = theSchema p `scSeq` theSchema q
    }

instance MonoidalAlt PU where
  zero = PU
    { appPickle = \a _ -> absurd a
    , appUnPickle = throwMsg "PU.zero"
    , theSchema = scNull
    }
  p >|< q = PU
    { appPickle = either (appPickle p) (appPickle q)
    , appUnPickle = mchoice (Left <$> appUnPickle p) return (Right <$> appUnPickle q)
    , theSchema = theSchema p `scAlt` theSchema q
    }

-- |Ignore any whitespace and produce nothing
xpWhitespace :: PU ()
xpWhitespace = PU
  { appPickle = const id
  , appUnPickle = modify $ \s -> s{ contents = dropWhile (any (all isXmlSpaceChar) . XN.getText) $ contents s }
  , theSchema = scEmpty
  }

-- |Ignore leading whitespace
xpTrim :: PU a -> PU a
xpTrim = (xpWhitespace *<)

-- |Like 'xpTrees' but more efficient
xpAnyCont :: PU HXT.XmlTrees
xpAnyCont = PU
  { appPickle = \c s -> s{ contents = c ++ contents s }
  , appUnPickle = state $ \s -> (contents s, s{ contents = [] })
  , theSchema = Any -- XXX
  }

-- |All attributes
xpAnyAttrs :: PU HXT.XmlTrees
xpAnyAttrs = PU
  { appPickle = \a s -> s{ attributes = a ++ attributes s }
  , appUnPickle = state $ \s -> (attributes s, s{ attributes = [] })
  , theSchema = Any -- XXX
  }

-- |Any content and attributes: combine 'xpAnyCont' and 'xpAnyAttrs'
xpAny :: PU HXT.XmlTrees
xpAny = (uncurry (++) Inv.:<->: partition XN.isAttr) >$< (xpAnyAttrs >*< xpAnyCont)

-- |Any single element
xpAnyElem :: PU HXT.XmlTree
xpAnyElem = xpWrapEither 
  ( \e -> if XN.isElem e then Right e else Left "xpAnyElem: any element expected"
  , id
  ) xpTree
