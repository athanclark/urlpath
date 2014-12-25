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

-- * Classes

-- | @Url@ is a relationship between an underlying (monomorphic) string type
-- @plain@, and a deployment context @m@. We try to make the deployment style
-- coercible at the top level - if the expression has a type
-- @Url String (AbsoluteUrlT String Identity)@ or
-- @Monad m => Url T.Text (GroundedUrlT LT.Text m)@ will force
-- /all use-cases within the expression/ to coerce to that type.
class ( IsString plain
      , Monoid plain
      , MonadReader plain m
      ) => Url plain (m :: * -> *) where
  url :: UrlString plain -- ^ Url type, parameterized over a string type @plain@
      -> m plain         -- ^ Rendered Url in some context.
  plainUrl :: plain   -- ^ raw small string
           -> m plain -- ^ Rendered string in some context.


-- | Overload deployment schemes with this - then, all that's needed is a type
-- coercion to change deployment.
class Url plain m => UrlReader plain m where
  type Result m :: * -> *
  runUrlReader :: Url plain m =>
                  m b -- ^ MonadReader with index @string@ and result @b@
               -> plain -- ^ Reader index
               -> Result m b -- ^ Final result


instance ( Monad m
         , Monoid plain
         , IsString plain ) => Url plain (RelativeUrlT plain m) where
  url = RelativeUrlT . const . return . expandRelative
  plainUrl x = RelativeUrlT $ const $ return $ expandRelative $ UrlString x []

instance ( Monad m
         , Monoid plain
         , IsString plain ) => UrlReader plain (RelativeUrlT plain m) where
  type Result (RelativeUrlT plain m) = m
  runUrlReader = runRelativeUrlT

instance ( Monad m
         , Monoid plain
         , IsString plain ) => Url plain (GroundedUrlT plain m) where
  url = GroundedUrlT . const . return . expandGrounded
  plainUrl x = GroundedUrlT $ const $ return $ expandGrounded $ UrlString x []

instance ( Monad m
         , Monoid plain
         , IsString plain ) => UrlReader plain (GroundedUrlT plain m) where
  type Result (GroundedUrlT plain m) = m
  runUrlReader = runGroundedUrlT

instance ( Monad m
         , Monoid plain
         , IsString plain ) => Url plain (AbsoluteUrlT plain m) where
  url = expandAbsolute
  plainUrl x = expandAbsolute $ UrlString x []

instance ( Monad m
         , Monoid plain
         , IsString plain ) => UrlReader plain (AbsoluteUrlT plain m) where
  type Result (AbsoluteUrlT plain m) = m
  runUrlReader = runAbsoluteUrlT
