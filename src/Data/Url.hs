{-# LANGUAGE
    KindSignatures
  , MultiParamTypeClasses
  , FunctionalDependencies
  , TypeFamilies
  , TypeSynonymInstances
  , FlexibleInstances
  , DeriveFunctor
  #-}

module Data.Url where

import Path.Extended

import Data.Functor.Identity
import Control.Monad.Trans


-- * Classes

class MonadUrl b (m :: * -> *) where
  pathUrl   :: Path b t
            -> m String
  locUrl    :: Location b t
            -> m String
  symbolUrl :: ( ToLocation s b t
               ) => s
                 -> m String

instance ( MonadUrl b m
         , MonadTrans t
         , Monad m
         ) => MonadUrl b (t m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl
  symbolUrl = lift . symbolUrl


-- | Make an instance for your own stringless route type to use your symbols
-- instead of strings or @Path@.
class ToLocation a b t | a -> b t where
  toLocation :: a -> Location b t

-- | Overload extraction for deployment transformers.
class UrlReader m where
  type RunUrlReader m :: * -> *
  runUrlReader :: m a -- ^ MonadReader with index @string@ and result @b@
               -> UrlAuthority -- ^ URI Scheme, hostname, and other details
               -> RunUrlReader m a -- ^ Final result


-- * Types

-- | The hostname of a URL.
data UrlAuthority = UrlAuthority
  { urlScheme  :: String
  , urlSlashes :: Bool
  , urlAuth    :: Maybe UrlAuthent
  , urlHost    :: String
  , urlPort    :: Maybe Int
  } deriving (Eq, Ord)

instance Show UrlAuthority where
  show (UrlAuthority sh sl ma h mp) =
      sh ++ ":"
   ++ if sl then "//" else ""
   ++ maybe "" (\a -> show a ++ "@") ma
   ++ h
   ++ maybe "" (\p -> ":" ++ show p) mp

data UrlAuthent = UrlAuthent
  { urlAuthUser :: String
  , urlAuthPass :: Maybe String
  } deriving (Eq, Ord)

instance Show UrlAuthent where
  show (UrlAuthent u mp) = u ++ maybe "" (\p -> ":" ++ p) mp


-- ** Relative Urls

newtype RelativeUrlT m a = RelativeUrlT
  { runRelativeUrlT :: UrlAuthority -> m a
  } deriving Functor

type RelativeUrl = RelativeUrlT Identity

instance Applicative m => Applicative (RelativeUrlT m) where
  pure x = RelativeUrlT $ const (pure x)
  f <*> x = RelativeUrlT $ \r ->
    (runRelativeUrlT f r) <*> (runRelativeUrlT x r)

instance Monad m => Monad (RelativeUrlT m) where
  return x = RelativeUrlT $ const (return x)
  m >>= f = RelativeUrlT $ \r ->
    runRelativeUrlT m r >>= (\x -> runRelativeUrlT (f x) r)

instance MonadTrans RelativeUrlT where
  lift = RelativeUrlT . const

instance MonadIO m => MonadIO (RelativeUrlT m) where
  liftIO = lift . liftIO

instance ( Applicative m
         ) => MonadUrl Rel (RelativeUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)
  symbolUrl x = pure (show (toLocation x))

instance UrlReader (RelativeUrlT m) where
  type RunUrlReader (RelativeUrlT m) = m
  runUrlReader = runRelativeUrlT


-- ** Grounded Urls

newtype GroundedUrlT m a = GroundedUrlT
  { runGroundedUrlT :: UrlAuthority -> m a
  } deriving Functor

type GroundedUrl = GroundedUrlT Identity

instance Applicative m => Applicative (GroundedUrlT m) where
  pure x = GroundedUrlT $ const (pure x)
  f <*> x = GroundedUrlT $ \r ->
    (runGroundedUrlT f r) <*> (runGroundedUrlT x r)

instance Monad m => Monad (GroundedUrlT m) where
  return x = GroundedUrlT $ const (return x)
  m >>= f = GroundedUrlT $ \r ->
    runGroundedUrlT m r >>= (\x -> runGroundedUrlT (f x) r)

instance MonadTrans GroundedUrlT where
  lift = GroundedUrlT . const

instance MonadIO m => MonadIO (GroundedUrlT m) where
  liftIO = lift . liftIO

instance ( Applicative m
         ) => MonadUrl Abs (GroundedUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)
  symbolUrl x = pure (show (toLocation x))

instance UrlReader (GroundedUrlT m) where
  type RunUrlReader (GroundedUrlT m) = m
  runUrlReader = runGroundedUrlT


-- ** Absolute Urls

newtype AbsoluteUrlT m a = AbsoluteUrlT
  { runAbsoluteUrlT :: UrlAuthority -> m a
  } deriving Functor

type AbsoluteUrl = AbsoluteUrlT Identity

instance Applicative m => Applicative (AbsoluteUrlT m) where
  pure x = AbsoluteUrlT $ const (pure x)
  f <*> x = AbsoluteUrlT $ \r ->
    (runAbsoluteUrlT f r) <*> (runAbsoluteUrlT x r)

instance Monad m => Monad (AbsoluteUrlT m) where
  return x = AbsoluteUrlT $ const (return x)
  m >>= f = AbsoluteUrlT $ \r ->
    runAbsoluteUrlT m r >>= (\x -> runAbsoluteUrlT (f x) r)

instance MonadTrans AbsoluteUrlT where
  lift = AbsoluteUrlT . const

instance MonadIO m => MonadIO (AbsoluteUrlT m) where
  liftIO = lift . liftIO

instance ( Applicative m
         ) => MonadUrl Abs (AbsoluteUrlT m) where
  pathUrl x   = AbsoluteUrlT (\h -> pure $ show h ++ toFilePath x)
  locUrl x    = AbsoluteUrlT (\h -> pure $ show h ++ show x)
  symbolUrl x = AbsoluteUrlT (\h -> pure $ show h ++ show (toLocation x))

instance UrlReader (AbsoluteUrlT m) where
  type RunUrlReader (AbsoluteUrlT m) = m
  runUrlReader = runAbsoluteUrlT
