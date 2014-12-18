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
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader.Class

-- | A GET parameter encoded in a Url.
data GETParam a where
  GETParam :: IsString a =>
              a
           -> a
           -> GETParam a

-- | Render a GET parameter pair.
renderGETParam :: ( IsString a
                  , Monoid a ) =>
                  GETParam a
               -> a
renderGETParam !(GETParam !k !v) =
  "&" <> k <> "=" <> v

-- | A Url string - a target page and GET parameters.
data UrlString a where
  UrlString :: IsString a =>  
               a
            -> [GETParam a]
            -> UrlString a

-- | Render a Url String /simply/ - this is equivalent to @expandRelative@.
renderUrlString :: ( IsString a
                   , Monoid a ) =>
                   UrlString a
                -> a
renderUrlString !(UrlString !t []) = t
renderUrlString !(UrlString !t [GETParam !k !v]) =
  t <> "?" <> k <> "=" <> v
renderUrlString !(UrlString !t (GETParam !k !v : ps)) =
  t <> "?" <> k <> "=" <> v <>
    foldr (\x acc -> acc <> renderGETParam x) "" ps


-- | Lifts a target path with some GET parameter chunks into a @UrlString@.
(<?>) :: ( IsString a
         , Monoid a ) =>
         a -- ^ Target string
      -> (a, a) -- ^ GET Parameter
      -> UrlString a
(<?>) !t (!k,!v) = UrlString t [GETParam k v]

infixl 9 <?>

-- | Adds more GET parameters to a @UrlString@.
(<&>) :: ( IsString a
         , Monoid a ) =>
         UrlString a -- ^ Old Url
      -> (a, a) -- ^ Additional GET Parameter
      -> UrlString a
(<&>) !(UrlString !t !p) (!k,!v) = UrlString t $ p ++ [GETParam k v]

infixl 8 <&>


-- | Render the Url String as relative
expandRelative :: ( IsString a
                  , Monoid a ) =>
                  UrlString a
               -> a
expandRelative = renderUrlString

-- | Render the Url String as grounded
expandGrounded :: ( IsString a
                  , Monoid a ) =>
                  UrlString a
               -> a
expandGrounded x = "/" <> renderUrlString x

-- | Render the Url String as absolute - getting the root from a MonadReader.
expandAbsolute :: ( IsString a
                  , Monoid a
                  , MonadReader a m) =>
                  UrlString a
               -> m a
expandAbsolute x = do
  host <- ask
  return $ host <> "/" <> renderUrlString x

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
expandAbsoluteWith :: ( IsString a
                      , Monoid a
                      , MonadReader a m) =>
                      UrlString a
                   -> (a -> a)
                   -> m a
expandAbsoluteWith x f = do
  root <- liftM f ask
  return $ root <> "/" <> renderUrlString x

-- | Rendering mode transformer. This isn't an instance of @UrlReader@ - to use, 
-- simple @lift@ as many levels as you need:
-- 
-- > foo :: Monad m => HtmlT (RelativeUrlT m) ()
-- > foo = do
-- >   url <- lift $ renderUrl $ "foo.php" <?> ("bar","baz")
-- >   script_ [src_ url] ""
--
-- When rendering @foo@, simply use the Transformer's @run@ function to convert 
-- it to a reader:
--
-- > bar :: Monad m => m LT.Text
-- > bar = (runRelativeUrlT (renderTextT foo)) "example.com"
--
-- It is generally simpler (but more restrictive) to use the non-transformer 
-- variety.
newtype RelativeUrlT h m a = RelativeUrlT { runRelativeUrlT :: h -> m a }
  deriving Functor

instance Applicative f => Applicative (RelativeUrlT h f) where
  (<*>) f x = RelativeUrlT $ \a ->
    (<*>) (runRelativeUrlT f a) (runRelativeUrlT x a)

instance Monad m => Monad (RelativeUrlT h m) where
  return x = RelativeUrlT $ \_ -> return x
  m >>= f = RelativeUrlT $ \a ->
    runRelativeUrlT m a >>= (\x -> runRelativeUrlT (f x) a)

instance MonadTrans (RelativeUrlT h) where
  lift m = RelativeUrlT (const m)

instance ( Monad m
         , IsString a ) => MonadReader a (RelativeUrlT a m) where
  ask = return ""

-- | Concrete Monad for automatically coercing HtmlT's to use a mode of Url 
-- rendering (relative, grounded, or absolute).
--
-- > foo :: HtmlT RelativeUrl ()
-- > foo = do
-- >   url <- lift $ renderUrl $ "foo.php" <?> ("bar","baz")
-- >   script_ [src_ url] ""
--
-- when rendering these simple monads for automatic conversion via coercion, use 
-- the @runUrlReader@ member function of the @UrlReader@ typeclass:
--
-- > bar :: LT.Text
-- > bar = (runUrlReader (renderTextT foo)) "example.com"
--
-- To change the mode of rendering, simple change the coerced type of @foo@.
newtype RelativeUrl h a = RelativeUrl { runRelativeUrl :: h -> a }
  deriving Functor

instance Applicative (RelativeUrl h) where
  (<*>) f x = RelativeUrl $ \a ->
    runRelativeUrl f a (runRelativeUrl x a)

instance Monad (RelativeUrl h) where
  return x = RelativeUrl $ const x
  m >>= f = RelativeUrl $ \a ->
    (\y -> runRelativeUrl (f y) a) (runRelativeUrl m a)

instance IsString a => MonadReader a (RelativeUrl a) where
  ask = return ""

newtype GroundedUrlT h m a = GroundedUrlT { runGroundedUrlT :: h -> m a }

instance Functor f => Functor (GroundedUrlT h f) where
  fmap f x = GroundedUrlT $ \a ->
    fmap f (runGroundedUrlT x a)

instance Applicative f => Applicative (GroundedUrlT h f) where
  (<*>) f x = GroundedUrlT $ \a ->
    (<*>) (runGroundedUrlT f a) (runGroundedUrlT x a)

instance Monad m => Monad (GroundedUrlT h m) where
  return x = GroundedUrlT $ \_ -> return x
  m >>= f = GroundedUrlT $ \a ->
    runGroundedUrlT m a >>= (\x -> runGroundedUrlT (f x) a)

instance MonadTrans (GroundedUrlT h) where
  lift m = GroundedUrlT (const m)

instance ( Monad m
         , IsString a ) => MonadReader a (GroundedUrlT a m) where
  ask = return "/"

newtype GroundedUrl h a = GroundedUrl { runGroundedUrl :: h -> a }

instance Functor (GroundedUrl h) where
  fmap f x = GroundedUrl $ \a -> f $ runGroundedUrl x a

instance Applicative (GroundedUrl h) where
  (<*>) f x = GroundedUrl $ \a ->
    runGroundedUrl f a (runGroundedUrl x a)

instance Monad (GroundedUrl h) where
  return x = GroundedUrl $ const x
  m >>= f = GroundedUrl $ \a ->
    (\y -> runGroundedUrl (f y) a) (runGroundedUrl m a)

instance IsString a => MonadReader a (GroundedUrl a) where
  ask = return "/" 
  
newtype AbsoluteUrlT h m a = AbsoluteUrlT { runAbsoluteUrlT :: h -> m a }

instance Functor f => Functor (AbsoluteUrlT h f) where
  fmap f x = AbsoluteUrlT $ \a ->
    fmap f (runAbsoluteUrlT x a)

instance Applicative f => Applicative (AbsoluteUrlT h f) where
  (<*>) f x = AbsoluteUrlT $ \a ->
    (<*>) (runAbsoluteUrlT f a) (runAbsoluteUrlT x a)

instance Monad m => Monad (AbsoluteUrlT h m) where
  return x = AbsoluteUrlT $ const $ return x
  m >>= f = AbsoluteUrlT $ \a ->
    runAbsoluteUrlT m a >>= (\x -> runAbsoluteUrlT (f x) a)

instance MonadTrans (AbsoluteUrlT h) where
  lift m = AbsoluteUrlT (const m)

instance ( Monad m
         , IsString a ) => MonadReader a (AbsoluteUrlT a m) where
  ask = AbsoluteUrlT return

newtype AbsoluteUrl h a = AbsoluteUrl { runAbsoluteUrl :: h -> a }

instance Functor (AbsoluteUrl h) where
  fmap f x = AbsoluteUrl $ \a -> f $ runAbsoluteUrl x a

instance Applicative (AbsoluteUrl h) where
  (<*>) f x = AbsoluteUrl $ \a ->
    runAbsoluteUrl f a (runAbsoluteUrl x a)

instance Monad (AbsoluteUrl h) where
  return x = AbsoluteUrl $ const x
  m >>= f = AbsoluteUrl $ \a ->
    (\y -> runAbsoluteUrl (f y) a) (runAbsoluteUrl m a)

instance IsString a => MonadReader a (AbsoluteUrl a) where
  ask = AbsoluteUrl id
