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



-- | @Url@ is a relationship between an underlying (monomorphic) string type
-- @string@, and a deployment context @m@. We try to make the deployment and 
-- implementation type coercible at the top level - coercing your final 
-- expression to @String@ or @T.Text@ will have /all/ use-cases coerce to that 
-- type, similarly with the deployment scheme.
--
-- We chose to not force @MonadReader@ as a superclass for @m@ due to the 
-- monomorphic demand on functional dependencies.
class ( IsString string, Monoid string, MonadReader string m ) =>
          Url
            string -- ^ Implementation string type
            (m :: * -> *) -- ^ Context for deployment
            where
  url :: UrlString string -- ^ Url type
      -> m string -- ^ Rendered Url in some context.
  stringUrl :: string -- ^ raw string
            -> m string -- ^ Rendered string in some context.

-- | Overload deployment schemes with this - then, all that's needed is a type 
-- coercion to change deployment. This only works with flat (co)monads, so monad 
-- transformers are out.
class Url string m => UrlReader string m where
  runUrlReader :: Url string m =>
                  m string -- ^ Monad with result @string@
               -> string -- ^ Reader index
               -> string -- ^ Final result


-- * Monads

instance ( Monoid a
         , IsString a ) => Url a (RelativeUrl a) where
  url = RelativeUrl . const . expandRelative
  stringUrl x = RelativeUrl $ const $ expandRelative $ UrlString x []

instance ( Monoid a
         , IsString a ) => UrlReader a (RelativeUrl a) where
  runUrlReader = runRelativeUrl

---

instance ( Monoid a
         , IsString a ) => Url a (GroundedUrl a) where
  url = GroundedUrl . const . expandGrounded
  stringUrl x = GroundedUrl $ const $ expandGrounded $ UrlString x []

instance ( Monoid a
         , IsString a ) => UrlReader a (GroundedUrl a) where
  runUrlReader = runGroundedUrl

---

instance ( Monoid a
         , IsString a ) => Url a (AbsoluteUrl a) where
  url = expandAbsolute
  stringUrl x = expandAbsolute $ UrlString x []

-- | Hand-off host prepending to the MonadReader instance
instance ( Monoid a
         , IsString a ) => UrlReader a (AbsoluteUrl a) where
  runUrlReader = runAbsoluteUrl


-- * Transformers

instance ( Monad m
         , Monoid a
         , IsString a ) => Url a (RelativeUrlT a m) where
  url = RelativeUrlT . const . return . expandRelative
  stringUrl x = RelativeUrlT $ const $ return $ expandRelative $ UrlString x []

instance ( Monad m
         , Monoid a
         , IsString a ) => Url a (GroundedUrlT a m) where
  url = GroundedUrlT . const . return . expandGrounded
  stringUrl x = GroundedUrlT $ const $ return $ expandGrounded $ UrlString x []

instance ( Monad m
         , Monoid a
         , IsString a ) => Url a (AbsoluteUrlT a m) where
  url = expandAbsolute
  stringUrl x = expandAbsolute $ UrlString x []

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
