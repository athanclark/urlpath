{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module UrlPath
    ( UrlReader (..)
    , Url (..)
    , module UrlPath.Types ) where

import UrlPath.Types

import Data.String
import Data.Monoid
import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Reader.Class


-- | We need a @MonadReader@ without the functional dependency. Luckily, our 
-- only use case is an OverloadedString!
class Monad m => StringReader (m :: * -> *) where
  askString :: IsString a =>
               m a

-- | Convenience typeclass for a uniform interface into pure @Reader@-like 
-- monads.
class UrlReader (m :: * -> *) where
  runUrlReader :: IsString a =>
                  m b -- ^ Monadic reader-like computation
               -> a -- ^ @IsString@ index
               -> b -- ^ Result

instance StringReader Identity where
  askString = return ""

instance UrlReader Identity where
  runUrlReader x _ = runIdentity x

instance IsString a => UrlReader (AbsoluteUrl a) where
  runUrlReader = runAbsoluteUrl

instance IsString a => UrlReader (RelativeUrl a) where
  runUrlReader = runRelativeUrl

instance IsString a => UrlReader (GroundedUrl a) where
  runUrlReader = runGroundedUrl

-- | @Url@ takes an input type @a@, and returns a modality @f@ around @T.Text@.
class Url a m where
  renderUrl ::
               a -- ^ Url-like type (@UrlString@ or @T.Text@).
            -> m b -- ^ Rendered Url in some context @f@

instance IsString a => Url a Identity where
  renderUrl = Identity

instance IsString a => Url (UrlString a) Identity where
  renderUrl = Identity . expandRelative

instance IsString a => Url (UrlString a) (RelativeUrl a) where
  renderUrl x = RelativeUrl $ \_ -> expandRelative x

instance IsString a => Url (UrlString a) (GroundedUrl a) where
  renderUrl x = GroundedUrl $ \_ -> expandGrounded x

instance IsString a => Url (UrlString a) (AbsoluteUrl a) where
  renderUrl = expandAbsolute

instance ( Monad m
         , IsString a ) => Url (UrlString a) (RelativeUrlT a m) where
  renderUrl x = RelativeUrlT $ \_ -> return $ expandRelative x

instance ( Monad m
         , IsString a ) => Url (UrlString a) (GroundedUrlT a m) where
  renderUrl x = GroundedUrlT $ \_ -> return $ expandGrounded x

instance ( Monad m
         , IsString a ) => Url (UrlString a) (AbsoluteUrlT a m) where
  renderUrl = expandAbsolute

instance IsString a => Url a (RelativeUrl a) where
  renderUrl x = RelativeUrl $ \_ -> expandRelative $ UrlString x []

instance IsString a => Url a (GroundedUrl a) where
  renderUrl x = GroundedUrl $ \_ -> expandGrounded $ UrlString x []

instance IsString a => Url a (AbsoluteUrl a) where
  renderUrl x = expandAbsolute $ UrlString x []

instance ( Monad m
         , IsString a ) => Url a (RelativeUrlT a m) where
  renderUrl x = RelativeUrlT $ \_ -> return $ expandRelative $ UrlString x []

instance ( Monad m
         , IsString a ) => Url a (GroundedUrlT a m) where
  renderUrl x = GroundedUrlT $ \_ -> return $ expandGrounded $ UrlString x []

instance ( Monad m
         , IsString a ) => Url a (AbsoluteUrlT a m) where
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
