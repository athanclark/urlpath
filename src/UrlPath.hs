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
class ( IsString plain, Monoid plain, MonadReader plain (m plain) ) =>
          Url plain (m :: * -> * -> *) where
  url :: UrlString plain -- ^ Url type, parameterized over /small/ string type @string@
      -> m plain plain -- ^ Rendered Url in some context.
  plainUrl :: plain -- ^ raw small string
            -> m plain plain -- ^ Rendered string in some context.

-- | Overload deployment schemes with this - then, all that's needed is a type 
-- coercion to change deployment. This only works with flat (co)monads, so monad 
-- transformers are out.
class Url plain m => UrlReader plain m where
  runUrlReader :: Url plain m =>
                  m plain b -- ^ MonadReader with index @string@ and result @b@
               -> plain -- ^ Reader index
               -> b -- ^ Final result

-- * Monads

instance ( Monoid plain
         , IsString plain ) => Url plain RelativeUrl where
  url = RelativeUrl . const . expandRelative
  plainUrl x = RelativeUrl $ const $ expandRelative $ UrlString x []

instance ( Monoid plain
         , IsString plain ) => UrlReader plain RelativeUrl where
  runUrlReader = runRelativeUrl

---

instance ( Monoid plain
         , IsString plain ) => Url plain GroundedUrl where
  url = GroundedUrl . const . expandGrounded
  plainUrl x = GroundedUrl $ const $ expandGrounded $ UrlString x []

instance ( Monoid plain
         , IsString plain ) => UrlReader plain GroundedUrl where
  runUrlReader = runGroundedUrl

---

instance ( Monoid plain
         , IsString plain ) => Url plain AbsoluteUrl where
  url = expandAbsolute
  plainUrl x = expandAbsolute $ UrlString x []

-- | Hand-off host prepending to the MonadReader instance
instance ( Monoid plain
         , IsString plain ) => UrlReader plain AbsoluteUrl where
  runUrlReader = runAbsoluteUrl

-- * Transformers

instance ( Monad m
         , Monoid plain
         , IsString plain ) => Url plain (RelativeUrlT m) where
  url = RelativeUrlT . const . return . expandRelative
  plainUrl x = RelativeUrlT $ const $ return $ expandRelative $ UrlString x []

instance ( Monad m
         , Monoid plain
         , IsString plain ) => Url plain (GroundedUrlT m) where
  url = GroundedUrlT . const . return . expandGrounded
  plainUrl x = GroundedUrlT $ const $ return $ expandGrounded $ UrlString x []

instance ( Monad m
         , Monoid plain
         , IsString plain ) => Url plain (AbsoluteUrlT m) where
  url = expandAbsolute
  plainUrl x = expandAbsolute $ UrlString x []
