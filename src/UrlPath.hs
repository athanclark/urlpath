{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}


module UrlPath
    ( UrlReader (..)
    , Url (..)
    , module UrlPath.Types ) where

import UrlPath.Types

import qualified Data.Text as T

import Control.Monad.Trans
import Control.Monad.Reader.Class

import Data.Monoid
import Control.Applicative
import Control.Monad
import Data.Functor.Identity


-- | Convenience typeclass for a uniform interface into pure @Reader@-like 
-- monads.
class MonadReader T.Text m => UrlReader (m :: * -> *) where
  runUrlReader :: m a -- ^ Monadic reader-like computation
               -> T.Text -- ^ @T.Text@ index
               -> a -- ^ Result

instance MonadReader T.Text Identity where
  ask = return ""

instance UrlReader Identity where
  runUrlReader x _ = runIdentity x

instance UrlReader AbsoluteUrl where
  runUrlReader = runAbsoluteUrl

instance UrlReader RelativeUrl where
  runUrlReader = runRelativeUrl

instance UrlReader GroundedUrl where
  runUrlReader = runGroundedUrl

-- | @Url@ takes an input type @a@, and returns a modality @f@ around @T.Text@.
class Monad m => Url a m where
  renderUrl :: a -- ^ Url-like type (@UrlString@ or @T.Text@).
            -> m T.Text -- ^ Rendered Url in some context @f@

instance Url T.Text Identity where
  renderUrl = Identity

instance Url UrlString Identity where
  renderUrl = Identity . expandRelative

instance Url UrlString RelativeUrl where
  renderUrl x = RelativeUrl $ \_ -> expandRelative x

instance Url UrlString GroundedUrl where
  renderUrl x = GroundedUrl $ \_ -> expandGrounded x

instance Url UrlString AbsoluteUrl where
  renderUrl = expandAbsolute

instance Monad m => Url UrlString (RelativeUrlT m) where
  renderUrl x = RelativeUrlT $ \_ -> return $ expandRelative x

instance Monad m => Url UrlString (GroundedUrlT m) where
  renderUrl x = GroundedUrlT $ \_ -> return $ expandGrounded x

instance Monad m => Url UrlString (AbsoluteUrlT m) where
  renderUrl = expandAbsolute

instance Url T.Text RelativeUrl where
  renderUrl x = RelativeUrl $ \_ -> expandRelative $ UrlString x []

instance Url T.Text GroundedUrl where
  renderUrl x = GroundedUrl $ \_ -> expandGrounded $ UrlString x []

instance Url T.Text AbsoluteUrl where
  renderUrl x = expandAbsolute $ UrlString x []

instance Monad m => Url T.Text (RelativeUrlT m) where
  renderUrl x = RelativeUrlT $ \_ -> return $ expandRelative $ UrlString x []

instance Monad m => Url T.Text (GroundedUrlT m) where
  renderUrl x = GroundedUrlT $ \_ -> return $ expandGrounded $ UrlString x []

instance Monad m => Url T.Text (AbsoluteUrlT m) where
  renderUrl x = expandAbsolute $ UrlString x []

-- * Example
--
-- | Here is an example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE ExtendedDefaultRules #-}
-- >
-- > import Lucid
-- > import Lucid.Path
-- > import qualified Data.Text as T
-- > import qualified Data.Text.Lazy as LT
-- >
-- > foo :: LT.Text
-- > foo = (runUrlReader $ renderTextT bar) "example.com"
-- >
-- > bar :: HtmlT AbsoluteUrl ()
-- > bar = do
-- >   url <- lift $ renderUrl $ "foo.php" <?> ("bar","baz")
-- >   script_ [src_ url] ""
--
-- Where @foo@ is now
--
-- > Lucid> foo
-- > "<script src=\"example.com/foo.php?bar=baz\"></script>"
