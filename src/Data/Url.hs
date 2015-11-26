{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , KindSignatures
  , FlexibleInstances
  , StandaloneDeriving
  , TypeSynonymInstances
  , UndecidableInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  #-}

-- |
-- Module      :  Data.Url
-- Copyright   :  (c) Athan L. Clark
-- License     :  MIT
--
-- Maintainer  :  Athan L. Clark <athan.clark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- This library helps us distinguish how we present URLs - we might show them
-- relatively, absolutely (with the URI authority - scheme, port, hostname, etc.),
-- or /grounded/ - where the path begins with @/@.
--
-- We leverage Chris Done's <https://hackage.haskell.org/package/path path>
-- library to distinguish relative vs. grounded paths at compile time, and provide
-- some additional features like a file extension and query parameters in
-- <https://hackage.haskell.org/package/path-extra path-extra>.

module Data.Url where

import Path.Extended

import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Monad.Morph



-- * Classes

-- | Turns a @Path@ or @Location@ into a @String@, where the rendering behavior
-- (relative, grounded and absolute) is encoded in the monad you use, much like
-- @LoggingT@ and @NoLoggingT@ from <https://hackage.haskell.org/package/monad-logger monad-logger>.
class MonadUrl base type' (m :: * -> *) where
  pathUrl   :: Path base type'
            -> m String
  locUrl    :: Location base type'
            -> m String

instance MonadUrl b t IO where
  pathUrl   = pure . toFilePath
  locUrl    = pure . show

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (MaybeT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (ListT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (ResourceT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (IdentityT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (LoggingT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (NoLoggingT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (ReaderT r m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         , Monoid w
         ) => MonadUrl b t (WriterT w m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (StateT s m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         , Error e
         ) => MonadUrl b t (ErrorT e m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (ContT r m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         ) => MonadUrl b t (ExceptT e m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl

instance ( MonadUrl b t m
         , Monad m
         , Monoid w
         ) => MonadUrl b t (RWST r w s m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl


-- * Types

-- | The hostname of a URL.
data UrlAuthority = UrlAuthority
  { urlScheme  :: String
  , urlSlashes :: Bool
  , urlAuth    :: Maybe UrlAuthent
  , urlHost    :: String
  , urlPort    :: Maybe Int
  } deriving (Show, Eq, Ord)

showUrlAuthority :: UrlAuthority -> String
showUrlAuthority (UrlAuthority sh sl ma h mp) =
      sh
   ++ ":"
   ++ (if sl then "//" else "")
   ++ maybe "" (\a -> showUrlAuthent a ++ "@") ma
   ++ h
   ++ maybe "" (\p -> ":" ++ show p) mp

data UrlAuthent = UrlAuthent
  { urlAuthUser :: String
  , urlAuthPass :: Maybe String
  } deriving (Show, Eq, Ord)

showUrlAuthent :: UrlAuthent -> String
showUrlAuthent (UrlAuthent u mp) =
  u ++ maybe "" (\p -> ":" ++ p) mp


-- ** Relative Urls

newtype RelativeUrlT m a = RelativeUrlT
  { runRelativeUrlT :: m a
  } deriving ( Show, Eq, Ord, Functor, Applicative, Alternative, Monad, MonadFix
             , MonadPlus, MonadIO, MonadReader r, MonadWriter w, MonadState s
             , MonadRWS r w s, MonadCont, MonadError e, MonadBase b, MonadThrow
             , MonadCatch, MonadMask, MonadLogger)

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (RelativeUrlT m)

type RelativeUrl = RelativeUrlT Identity

instance MonadTrans RelativeUrlT where
  lift = RelativeUrlT

instance MonadTransControl RelativeUrlT where
  type StT RelativeUrlT a = a
  liftWith f = RelativeUrlT (f (\t -> runRelativeUrlT t))
  restoreT = RelativeUrlT

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (RelativeUrlT m) where
  type StM (RelativeUrlT m) a = ComposeSt RelativeUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MFunctor RelativeUrlT where
  hoist f (RelativeUrlT x) = RelativeUrlT (f x)

instance MMonad RelativeUrlT where
  embed f x = f (runRelativeUrlT x)


instance ( Applicative m
         ) => MonadUrl Rel File (RelativeUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)

instance ( Applicative m
         ) => MonadUrl Rel Dir (RelativeUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)


-- ** Grounded Urls

newtype GroundedUrlT m a = GroundedUrlT
  { runGroundedUrlT :: m a
  } deriving ( Show, Eq, Ord, Functor, Applicative, Alternative, Monad, MonadFix
             , MonadPlus, MonadIO, MonadReader r, MonadWriter w, MonadState s
             , MonadRWS r w s, MonadCont, MonadError e, MonadBase b, MonadThrow
             , MonadCatch, MonadMask, MonadLogger)

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (GroundedUrlT m)

type GroundedUrl = GroundedUrlT Identity

instance MonadTrans GroundedUrlT where
  lift = GroundedUrlT

instance MonadTransControl GroundedUrlT where
  type StT GroundedUrlT a = a
  liftWith f = GroundedUrlT (f (\t -> runGroundedUrlT t))
  restoreT = GroundedUrlT

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (GroundedUrlT m) where
  type StM (GroundedUrlT m) a = ComposeSt GroundedUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MFunctor GroundedUrlT where
  hoist f (GroundedUrlT x) = GroundedUrlT (f x)

instance MMonad GroundedUrlT where
  embed f x = f (runGroundedUrlT x)

instance ( Applicative m
         ) => MonadUrl Abs File (GroundedUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)

instance ( Applicative m
         ) => MonadUrl Abs Dir (GroundedUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)


-- ** Absolute Urls

newtype AbsoluteUrlT m a = AbsoluteUrlT
  { runAbsoluteUrlT :: UrlAuthority -> m a
  } deriving Functor

type AbsoluteUrl = AbsoluteUrlT Identity

instance ( Applicative m
         ) => MonadUrl Abs File (AbsoluteUrlT m) where
  pathUrl x   = AbsoluteUrlT (\h -> pure $ showUrlAuthority h ++ toFilePath x)
  locUrl x    = AbsoluteUrlT (\h -> pure $ showUrlAuthority h ++ show x)

instance ( Applicative m
         ) => MonadUrl Abs Dir (AbsoluteUrlT m) where
  pathUrl x   = AbsoluteUrlT (\h -> pure $ showUrlAuthority h ++ toFilePath x)
  locUrl x    = AbsoluteUrlT (\h -> pure $ showUrlAuthority h ++ show x)

instance Applicative m => Applicative (AbsoluteUrlT m) where
  pure x = AbsoluteUrlT $ const (pure x)
  f <*> x = AbsoluteUrlT $ \r ->
    runAbsoluteUrlT f r <*> runAbsoluteUrlT x r

instance Alternative m => Alternative (AbsoluteUrlT m) where
  empty = AbsoluteUrlT (\_ -> empty)
  (AbsoluteUrlT f) <|> (AbsoluteUrlT g) = AbsoluteUrlT $ \h -> f h <|> g h

instance Monad m => Monad (AbsoluteUrlT m) where
  return x = AbsoluteUrlT $ const (return x)
  m >>= f = AbsoluteUrlT $ \r ->
    runAbsoluteUrlT m r >>= (\x -> runAbsoluteUrlT (f x) r)

instance MonadTrans AbsoluteUrlT where
  lift = AbsoluteUrlT . const

instance MonadIO m => MonadIO (AbsoluteUrlT m) where
  liftIO = lift . liftIO

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

instance ( MonadRWS r w s m
         ) => MonadRWS r w s (AbsoluteUrlT m) where

instance ( MonadCont m
         ) => MonadCont (AbsoluteUrlT m) where
  callCC f = AbsoluteUrlT $ \r ->
    callCC $ \c -> runAbsoluteUrlT (f (AbsoluteUrlT . const . c)) r

instance ( MonadError e m
         ) => MonadError e (AbsoluteUrlT m) where
  throwError = lift . throwError
  catchError (AbsoluteUrlT x) f = AbsoluteUrlT $ \r ->
    catchError (x r) (flip runAbsoluteUrlT r . f)

instance ( MonadBase b m
         ) => MonadBase b (AbsoluteUrlT m) where
  liftBase = liftBaseDefault

instance MonadTransControl AbsoluteUrlT where
  type StT AbsoluteUrlT a = a
  liftWith f = AbsoluteUrlT $ \r ->
    f $ \t -> runAbsoluteUrlT t r
  restoreT = AbsoluteUrlT . const

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (AbsoluteUrlT m) where
  type StM (AbsoluteUrlT m) a = ComposeSt AbsoluteUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance ( MonadThrow m
         ) => MonadThrow (AbsoluteUrlT m) where
  throwM = lift . throwM

instance ( MonadCatch m
         ) => MonadCatch (AbsoluteUrlT m) where
  catch (AbsoluteUrlT x) f = AbsoluteUrlT $ \r ->
    catch (x r) (flip runAbsoluteUrlT r . f)

instance ( MonadMask m
         ) => MonadMask (AbsoluteUrlT m) where
  mask a = AbsoluteUrlT $ \r ->
    mask $ \u -> runAbsoluteUrlT (a $ q u) r
    where q u (AbsoluteUrlT x) = AbsoluteUrlT (u . x)
  uninterruptibleMask a = AbsoluteUrlT $ \r ->
    uninterruptibleMask $ \u -> runAbsoluteUrlT (a $ q u) r
    where q u (AbsoluteUrlT x) = AbsoluteUrlT (u . x)

instance ( MonadLogger m
         ) => MonadLogger (AbsoluteUrlT m) where
  monadLoggerLog a b c d = lift (monadLoggerLog a b c d)

instance ( MonadResource m
         ) => MonadResource (AbsoluteUrlT m) where
  liftResourceT = lift . liftResourceT

instance MFunctor AbsoluteUrlT where
  hoist f (AbsoluteUrlT x) = AbsoluteUrlT $ \r ->
    f (x r)

instance MMonad AbsoluteUrlT where
  embed f x = AbsoluteUrlT $ \r ->
    runAbsoluteUrlT (f (runAbsoluteUrlT x r)) r
