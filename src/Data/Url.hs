{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , KindSignatures
  , FlexibleInstances
  , TypeSynonymInstances
  , UndecidableInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Data.Url where

import Path.Extended

import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State


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

instance ( MonadReader r m
         ) => MonadReader r (RelativeUrlT m) where
  ask       = lift ask
  local f (RelativeUrlT x) = RelativeUrlT $ \r ->
    local f (x r)

instance ( MonadWriter w m
         ) => MonadWriter w (RelativeUrlT m) where
  tell w = lift (tell w)
  listen (RelativeUrlT x) = RelativeUrlT $ \r ->
    listen (x r)
  pass (RelativeUrlT x) = RelativeUrlT $ \r ->
    pass (x r)

instance ( MonadState s m
         ) => MonadState s (RelativeUrlT m) where
  get   = lift get
  put x = lift (put x)


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

instance ( MonadReader r m
         ) => MonadReader r (GroundedUrlT m) where
  ask       = lift ask
  local f (GroundedUrlT x) = GroundedUrlT $ \r ->
    local f (x r)

instance ( MonadWriter w m
         ) => MonadWriter w (GroundedUrlT m) where
  tell w = lift (tell w)
  listen (GroundedUrlT x) = GroundedUrlT $ \r ->
    listen (x r)
  pass (GroundedUrlT x) = GroundedUrlT $ \r ->
    pass (x r)

instance ( MonadState s m
         ) => MonadState s (GroundedUrlT m) where
  get   = lift get
  put x = lift (put x)


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

instance ( MonadReader r m
         ) => MonadReader r (AbsoluteUrlT m) where
  ask       = lift ask
  local f (AbsoluteUrlT x) = AbsoluteUrlT $ \r ->
    local f (x r)

instance ( MonadWriter w m
         ) => MonadWriter w (AbsoluteUrlT m) where
  tell w = lift (tell w)
  listen (AbsoluteUrlT x) = AbsoluteUrlT $ \r ->
    listen (x r)
  pass (AbsoluteUrlT x) = AbsoluteUrlT $ \r ->
    pass (x r)

instance ( MonadState s m
         ) => MonadState s (AbsoluteUrlT m) where
  get   = lift get
  put x = lift (put x)
