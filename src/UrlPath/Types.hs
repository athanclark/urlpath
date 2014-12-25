{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module UrlPath.Types where

import Data.String
import Data.Monoid
import Data.Functor.Identity
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader.Class

-- | Abstract data type for a Url - a "target" and GET parameters. We require @IsString@
-- and @Monoid@ for generic construction, but rendering will require a monomorphic type.
--
-- The type constructor is parameterized over it's underlying @IsString@ & 
-- @Monoid@ instance.
data UrlString a where
  UrlString :: ( IsString a
               , Monoid a ) =>
               a
            -> [(a, a)]
            -> UrlString a

-- | We can't provide a @Show@ instance for @UrlString@ because that would force 
-- us to use @String@.
showUrlString :: UrlString a
              -> a
showUrlString (UrlString !t []) = t
showUrlString (UrlString !t ((!k,!v):xs)) =
  t <> "?" <> k <> "=" <> v <>
    foldl (\acc (x,y) -> acc <> "&" <> x <> "=" <> y) "" xs


-- | Makes a @UrlString@ out of a raw target path and a GET parameter pair.
(<?>) :: ( IsString a
         , Monoid a ) =>
         a      -- ^ Target string
      -> (a, a) -- ^ GET Parameter
      -> UrlString a
(<?>) !t !kv = UrlString t [kv]

infixl 9 <?>

-- | Adds another GET parameter pair to a @UrlString@.
(<&>) :: ( IsString a
         , Monoid a ) =>
         UrlString a -- ^ Old Url
      -> (a, a)      -- ^ Additional GET Parameter
      -> UrlString a
(<&>) (UrlString !t !p) !kv = UrlString t $ p ++ [kv]

infixl 8 <&>


-- | Render the Url String flatly - without anything prepended to the target.
expandRelative :: ( IsString plain
                  , Monoid plain ) =>
                  UrlString plain
               -> plain
expandRelative = showUrlString

-- | Render the Url String as grounded - prepended with a "root" @//@ character.
expandGrounded :: ( IsString plain
                  , Monoid plain ) =>
                  UrlString plain
               -> plain
expandGrounded !x = "/" <> showUrlString x

-- | Render the Url String as absolute - getting the root from a @MonadReader@ 
-- context.
expandAbsolute :: ( MonadReader plain m
                  , IsString plain
                  , Monoid plain ) =>
                  UrlString plain
               -> m plain
expandAbsolute !x = do
  host <- ask
  return $ host <> "/" <> showUrlString x

-- | Render the Url String as absolute, but with your own configuration type.
--
-- > data SiteConfig = SiteConfig { host :: T.Text
-- >                              , cdnHost :: T.Text
-- >                              }
-- >   deriving (Show, Eq)
-- >
-- > foo :: HtmlT (Reader SiteConfig) ()
-- > foo = do
-- >   url <- lift $ expandAbsoluteWith ("foo.php" <?> ("bar","baz")) host
-- >   script_ [src_ url] ""
-- >
-- > bar :: LT.Text 
-- > bar = (runReader (runTextT foo)) $
-- >   SiteConfig "example.com" "cdn.example.com"
expandAbsoluteWith :: ( MonadReader a m
                      , IsString plain
                      , Monoid plain ) =>
                      UrlString plain
                   -> (a -> plain)
                   -> m plain
expandAbsoluteWith !x f = do
  root <- liftM f ask
  return $ root <> "/" <> showUrlString x

newtype RelativeUrlT h m b = RelativeUrlT { runRelativeUrlT :: h -> m b }
  deriving Functor

instance Applicative f => Applicative (RelativeUrlT h f) where
  (<*>) f x = RelativeUrlT $ \a ->
    (<*>) (runRelativeUrlT f a) (runRelativeUrlT x a)

instance Monad m => Monad (RelativeUrlT h m) where
  return x = RelativeUrlT $ \_ -> return x
  m >>= f = RelativeUrlT $ \a ->
    runRelativeUrlT m a >>= (\x -> runRelativeUrlT (f x) a)

instance MonadTrans (RelativeUrlT h) where
  lift = RelativeUrlT . const

instance ( Monad m
         , IsString h ) => MonadReader h (RelativeUrlT h m) where
  ask = return ""

instance MonadIO m => MonadIO (RelativeUrlT h m) where
  liftIO = lift . liftIO

type RelativeUrl h b = RelativeUrlT h Identity b

newtype GroundedUrlT h m b = GroundedUrlT { runGroundedUrlT :: h -> m b }
  deriving Functor

instance Applicative f => Applicative (GroundedUrlT h f) where
  (<*>) f x = GroundedUrlT $ \a ->
    (<*>) (runGroundedUrlT f a) (runGroundedUrlT x a)

instance Monad m => Monad (GroundedUrlT h m) where
  return x = GroundedUrlT $ \_ -> return x
  m >>= f = GroundedUrlT $ \a ->
    runGroundedUrlT m a >>= (\x -> runGroundedUrlT (f x) a)

instance MonadTrans (GroundedUrlT h) where
  lift = GroundedUrlT . const

instance ( Monad m
         , IsString h ) => MonadReader h (GroundedUrlT h m) where
  ask = return "/"

instance MonadIO m => MonadIO (GroundedUrlT h m) where
  liftIO = lift . liftIO

type GroundedUrl h b = GroundedUrlT h Identity b

newtype AbsoluteUrlT h m b = AbsoluteUrlT { runAbsoluteUrlT :: h -> m b }
  deriving Functor

instance Applicative f => Applicative (AbsoluteUrlT h f) where
  (<*>) f x = AbsoluteUrlT $ \a ->
    (<*>) (runAbsoluteUrlT f a) (runAbsoluteUrlT x a)

instance Monad m => Monad (AbsoluteUrlT h m) where
  return x = AbsoluteUrlT $ const $ return x
  m >>= f = AbsoluteUrlT $ \a ->
    runAbsoluteUrlT m a >>= (\x -> runAbsoluteUrlT (f x) a)

instance MonadTrans (AbsoluteUrlT h) where
  lift = AbsoluteUrlT . const

instance ( Monad m
         , IsString h ) => MonadReader h (AbsoluteUrlT h m) where
  ask = AbsoluteUrlT return

instance MonadIO m => MonadIO (AbsoluteUrlT h m) where
  liftIO = lift . liftIO

type AbsoluteUrl h b = AbsoluteUrlT h Identity b
