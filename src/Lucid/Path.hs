{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}


module Lucid.Path where

import qualified Data.Text as T

import Control.Monad.Trans
import Control.Monad.Reader.Class

import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad
import Data.Functor.Identity

-- | @Url@ takes an input type @a@, and returns a modality @f@ around @T.Text@.
class Monad m => Url a m where
  renderUrl :: a -- ^ Url-like type (@UrlString@ or @T.Text@).
            -> m T.Text -- ^ Rendered Url in some context @f@

instance Url T.Text Identity where
  renderUrl = Identity

-- | A GET parameter encoded in a Url.
data GETParam = GETParam { key :: !T.Text -- ^ Key for a get parameter.
                         , val :: !T.Text -- ^ Value for the key.
                         }
  deriving (Show, Eq)

-- | Render a GET parameter pair.
renderGETParam :: GETParam
               -> T.Text
renderGETParam (GETParam k v) =
  "&" <> k <> "=" <> v

-- | A Relative Url string with a phantom data type denoting the default 
-- expansion scheme.
data UrlString = UrlString { target :: !T.Text -- ^ Relative base file - eg) @"foo.php"@ in @"foo.php?bar=baz"@.
                           , params :: [GETParam] -- ^ GET Parameters.
                           }
  deriving (Show, Eq)

-- | Render a Url String /simply/ - this is equivalent to @expandRelative@.
renderUrlString :: UrlString
                -> T.Text
renderUrlString (UrlString t []) = t
renderUrlString (UrlString t [GETParam k v]) =
  t <> "?" <> k <> "=" <> v
renderUrlString (UrlString t (GETParam k v : ps)) =
  t <> "?" <> k <> "=" <> v <>
    foldr (\x acc -> acc <> renderGETParam x) "" ps


-- | Lifts a target path with some GET parameter chunks into a @UrlString@.
(<?>) :: T.Text -- ^ Target string
      -> (T.Text, T.Text) -- ^ GET Parameter
      -> UrlString
t <?> (k,v) = UrlString t [GETParam k v]

infixl 9 <?>

-- | Adds more GET parameters to a @UrlString@.
(<&>) :: UrlString -- ^ Old Url
      -> (T.Text, T.Text) -- ^ Additional GET Parameter
      -> UrlString
old <&> (k,v) = UrlString (target old) $ params old ++ [GETParam k v]

infixl 8 <&>

-- | Render the Url String as relative
expandRelative :: UrlString
               -> T.Text
expandRelative = renderUrlString

-- | Render the Url String as grounded
expandGrounded :: UrlString
               -> T.Text
expandGrounded x = "/" <> renderUrlString x

-- | Render the Url String as absolute - getting the root from a MonadReader.
expandAbsolute :: (MonadReader T.Text m) =>
                  UrlString
               -> m T.Text
expandAbsolute x = do
  root <- ask
  return $ root <> "/" <> renderUrlString x

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
expandAbsoluteWith :: (MonadReader a m) =>
                      UrlString
                   -> (a -> T.Text)
                   -> m T.Text
expandAbsoluteWith x f = do
  root <- liftM f ask
  return $ root <> "/" <> renderUrlString x

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
newtype RelativeUrlT m a = RelativeUrlT { runRelativeUrlT :: T.Text -> m a }

instance Functor f => Functor (RelativeUrlT f) where
  fmap f x = RelativeUrlT $ \a ->
    fmap f (runRelativeUrlT x a)

instance Applicative f => Applicative (RelativeUrlT f) where
  (<*>) f x = RelativeUrlT $ \a ->
    (<*>) (runRelativeUrlT f a) (runRelativeUrlT x a)

instance Monad m => Monad (RelativeUrlT m) where
  return x = RelativeUrlT $ \_ -> return x
  m >>= f = RelativeUrlT $ \a ->
    runRelativeUrlT m a >>= (\x -> runRelativeUrlT (f x) a)

instance MonadTrans RelativeUrlT where
  lift m = RelativeUrlT (const m)

instance Monad m => MonadReader T.Text (RelativeUrlT m) where
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
newtype RelativeUrl a = RelativeUrl { runRelativeUrl :: T.Text -> a }

instance Functor RelativeUrl where
  fmap f x = RelativeUrl $ \a -> f $ runRelativeUrl x a

instance Applicative RelativeUrl where
  (<*>) f x = RelativeUrl $ \a ->
    runRelativeUrl f a (runRelativeUrl x a)

instance Monad RelativeUrl where
  return x = RelativeUrl $ const x
  m >>= f = RelativeUrl $ \a ->
    (\y -> runRelativeUrl (f y) a) (runRelativeUrl m a)

instance MonadReader T.Text RelativeUrl where
  ask = return ""

instance UrlReader RelativeUrl where
  runUrlReader = runRelativeUrl

newtype GroundedUrlT m a = GroundedUrlT { runGroundedUrlT :: T.Text -> m a }

instance Functor f => Functor (GroundedUrlT f) where
  fmap f x = GroundedUrlT $ \a ->
    fmap f (runGroundedUrlT x a)

instance Applicative f => Applicative (GroundedUrlT f) where
  (<*>) f x = GroundedUrlT $ \a ->
    (<*>) (runGroundedUrlT f a) (runGroundedUrlT x a)

instance Monad m => Monad (GroundedUrlT m) where
  return x = GroundedUrlT $ \_ -> return x
  m >>= f = GroundedUrlT $ \a ->
    runGroundedUrlT m a >>= (\x -> runGroundedUrlT (f x) a)

instance MonadTrans GroundedUrlT where
  lift m = GroundedUrlT (const m)

instance Monad m => MonadReader T.Text (GroundedUrlT m) where
  ask = return "/"

newtype GroundedUrl a = GroundedUrl { runGroundedUrl :: T.Text -> a }

instance Functor GroundedUrl where
  fmap f x = GroundedUrl $ \a -> f $ runGroundedUrl x a

instance Applicative GroundedUrl where
  (<*>) f x = GroundedUrl $ \a ->
    runGroundedUrl f a (runGroundedUrl x a)

instance Monad GroundedUrl where
  return x = GroundedUrl $ const x
  m >>= f = GroundedUrl $ \a ->
    (\y -> runGroundedUrl (f y) a) (runGroundedUrl m a)

instance MonadReader T.Text GroundedUrl where
  ask = return "/"

instance UrlReader GroundedUrl where
  runUrlReader = runGroundedUrl

newtype AbsoluteUrlT m a = AbsoluteUrlT { runAbsoluteUrlT :: T.Text -> m a }

instance Functor f => Functor (AbsoluteUrlT f) where
  fmap f x = AbsoluteUrlT $ \a ->
    fmap f (runAbsoluteUrlT x a)

instance Applicative f => Applicative (AbsoluteUrlT f) where
  (<*>) f x = AbsoluteUrlT $ \a ->
    (<*>) (runAbsoluteUrlT f a) (runAbsoluteUrlT x a)

instance Monad m => Monad (AbsoluteUrlT m) where
  return x = AbsoluteUrlT $ const $ return x
  m >>= f = AbsoluteUrlT $ \a ->
    runAbsoluteUrlT m a >>= (\x -> runAbsoluteUrlT (f x) a)

instance MonadTrans AbsoluteUrlT where
  lift m = AbsoluteUrlT (const m)

instance Monad m => MonadReader T.Text (AbsoluteUrlT m) where
  ask = AbsoluteUrlT return

newtype AbsoluteUrl a = AbsoluteUrl { runAbsoluteUrl :: T.Text -> a }

instance Functor AbsoluteUrl where
  fmap f x = AbsoluteUrl $ \a -> f $ runAbsoluteUrl x a

instance Applicative AbsoluteUrl where
  (<*>) f x = AbsoluteUrl $ \a ->
    runAbsoluteUrl f a (runAbsoluteUrl x a)

instance Monad AbsoluteUrl where
  return x = AbsoluteUrl $ const x
  m >>= f = AbsoluteUrl $ \a ->
    (\y -> runAbsoluteUrl (f y) a) (runAbsoluteUrl m a)

instance MonadReader T.Text AbsoluteUrl where
  ask = AbsoluteUrl id

instance UrlReader AbsoluteUrl where
  runUrlReader = runAbsoluteUrl

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
