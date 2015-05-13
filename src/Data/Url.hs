{-# LANGUAGE
    OverloadedStrings
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , KindSignatures
  , InstanceSigs
  , TypeFamilies
  , RankNTypes
  #-}

module Data.Url
    ( UrlReader (..)
    , Url (..)
    , module Data.Url.Types ) where

import Data.Url.Types

import Data.String
import Data.Monoid
import Data.Monoid.Textual (TextualMonoid)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader.Class

-- * Classes

-- | @Url@ is a relationship between an underlying string type
-- @plain@, and a deployment context @m@. We try to make the deployment style
-- coercible at the top level - if the expression has a type
-- @Url String (AbsoluteUrlT String Identity)@ or
-- @Monad m => Url T.Text (GroundedUrlT LT.Text m)@ will force
-- /all use-cases within the expression/ to coerce to that type.
class ( TextualMonoid plain
      , MonadReader plain m
      ) => Url plain (m :: * -> *) where
  queryUrl :: QueryString plain -- ^ Url type, parameterized over a string type @plain@
           -> m plain           -- ^ Rendered Url in some context.
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
         , TextualMonoid plain ) => Url plain (RelativeUrlT plain m) where
  queryUrl = RelativeUrlT . const . return . expandRelative
  plainUrl x = RelativeUrlT $ const $ return $ expandRelative $ QueryString x [] Nothing

instance ( Monad m
         , TextualMonoid plain ) => UrlReader plain (RelativeUrlT plain m) where
  type Result (RelativeUrlT plain m) = m
  runUrlReader = runRelativeUrlT

instance ( Monad m
         , TextualMonoid plain ) => Url plain (GroundedUrlT plain m) where
  queryUrl = GroundedUrlT . const . return . expandGrounded
  plainUrl x = GroundedUrlT $ const $ return $ expandGrounded $ QueryString x [] Nothing

instance ( Monad m
         , TextualMonoid plain ) => UrlReader plain (GroundedUrlT plain m) where
  type Result (GroundedUrlT plain m) = m
  runUrlReader = runGroundedUrlT

instance ( Monad m
         , TextualMonoid plain ) => Url plain (AbsoluteUrlT plain m) where
  queryUrl = expandAbsolute
  plainUrl x = expandAbsolute $ QueryString x [] Nothing

instance ( Monad m
         , TextualMonoid plain ) => UrlReader plain (AbsoluteUrlT plain m) where
  type Result (AbsoluteUrlT plain m) = m
  runUrlReader = runAbsoluteUrlT
