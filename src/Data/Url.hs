{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , KindSignatures
  , OverloadedStrings
  , FlexibleInstances
  , StandaloneDeriving
  , TypeSynonymInstances
  , UndecidableInstances
  , MultiParamTypeClasses
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
import Data.Functor.Compose
import Data.URI (URI (..))
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (Localhost))
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.Tuple as Strict
import qualified Data.Vector as V
import Data.List.Split (splitOn)
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.Trans.Error (Error, ErrorT)
import Control.Monad.Except
import Control.Monad.Trans.Control
import qualified Control.Monad.Trans.Control.Aligned as Aligned
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
            -> m URI
  locUrl    :: Location base type'
            -> m URI

instance MonadUrl b t IO where
  pathUrl x = pure (mkUriPathEmpty x)
  locUrl  x = pure (mkUriLocEmpty x)

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
  liftWith f = RelativeUrlT (f runRelativeUrlT)
  restoreT = RelativeUrlT

instance Aligned.MonadTransControl RelativeUrlT Identity where
  liftWith f = RelativeUrlT (f (\x -> Identity <$> runRelativeUrlT x))
  restoreT x = RelativeUrlT (runIdentity <$> x)

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (RelativeUrlT m) where
  type StM (RelativeUrlT m) a = ComposeSt RelativeUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance ( Aligned.MonadBaseControl b m stM
         ) => Aligned.MonadBaseControl b (RelativeUrlT m) (Compose stM Identity) where
  liftBaseWith = Aligned.defaultLiftBaseWith
  restoreM = Aligned.defaultRestoreM

instance MFunctor RelativeUrlT where
  hoist f (RelativeUrlT x) = RelativeUrlT (f x)

instance MMonad RelativeUrlT where
  embed f x = f (runRelativeUrlT x)


instance ( Applicative m
         ) => MonadUrl Rel File (RelativeUrlT m) where
  pathUrl x   = pure (mkUriPathEmpty x)
  locUrl x    = pure (mkUriLocEmpty x)

instance ( Applicative m
         ) => MonadUrl Rel Dir (RelativeUrlT m) where
  pathUrl x   = pure (mkUriPathEmpty x)
  locUrl x    = pure (mkUriLocEmpty x)


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
  liftWith f = GroundedUrlT (f runGroundedUrlT)
  restoreT = GroundedUrlT

instance Aligned.MonadTransControl GroundedUrlT Identity where
  liftWith f = GroundedUrlT (f (\x -> Identity <$> runGroundedUrlT x))
  restoreT x = GroundedUrlT (runIdentity <$> x)

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (GroundedUrlT m) where
  type StM (GroundedUrlT m) a = ComposeSt GroundedUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance ( Aligned.MonadBaseControl b m stM
         ) => Aligned.MonadBaseControl b (GroundedUrlT m) (Compose stM Identity) where
  liftBaseWith = Aligned.defaultLiftBaseWith
  restoreM = Aligned.defaultRestoreM

instance MFunctor GroundedUrlT where
  hoist f (GroundedUrlT x) = GroundedUrlT (f x)

instance MMonad GroundedUrlT where
  embed f x = f (runGroundedUrlT x)

instance ( Applicative m
         ) => MonadUrl Abs File (GroundedUrlT m) where
  pathUrl x   = pure (mkUriPathEmpty x)
  locUrl x    = pure (mkUriLocEmpty x)

instance ( Applicative m
         ) => MonadUrl Abs Dir (GroundedUrlT m) where
  pathUrl x   = pure (mkUriPathEmpty x)
  locUrl x    = pure (mkUriLocEmpty x)


-- ** Absolute Urls

newtype AbsoluteUrlT m a = AbsoluteUrlT
  { runAbsoluteUrlT :: URIAuth -> m a
  } deriving Functor

type AbsoluteUrl = AbsoluteUrlT Identity

instance ( Applicative m
         ) => MonadUrl Abs File (AbsoluteUrlT m) where
  pathUrl x   = AbsoluteUrlT (\h -> pure $ mkUriPath h x)
  locUrl x    = AbsoluteUrlT (\h -> pure $ mkUriLoc h x)

instance ( Applicative m
         ) => MonadUrl Abs Dir (AbsoluteUrlT m) where
  pathUrl x   = AbsoluteUrlT (\h -> pure $ mkUriPath h x)
  locUrl x    = AbsoluteUrlT (\h -> pure $ mkUriLoc h x)

instance Applicative m => Applicative (AbsoluteUrlT m) where
  pure x = AbsoluteUrlT $ const (pure x)
  f <*> x = AbsoluteUrlT $ \r ->
    runAbsoluteUrlT f r <*> runAbsoluteUrlT x r

instance Alternative m => Alternative (AbsoluteUrlT m) where
  empty = AbsoluteUrlT (const empty)
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

instance Aligned.MonadTransControl AbsoluteUrlT Identity where
  liftWith f = AbsoluteUrlT $ \r ->
    f $ \x -> Identity <$> runAbsoluteUrlT x r
  restoreT x = AbsoluteUrlT $ \_ -> runIdentity <$> x

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (AbsoluteUrlT m) where
  type StM (AbsoluteUrlT m) a = ComposeSt AbsoluteUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance ( Aligned.MonadBaseControl b m stM
         ) => Aligned.MonadBaseControl b (AbsoluteUrlT m) (Compose stM Identity) where
  liftBaseWith = Aligned.defaultLiftBaseWith
  restoreM = Aligned.defaultRestoreM

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

mkUriPath :: URIAuth -> Path base type' -> URI
mkUriPath auth path = URI (Strict.Just "https")
                     True
                     auth
                     (V.fromList $ fmap T.pack $ splitOn "/" $ toFilePath path)
                     V.empty
                     Strict.Nothing

mkUriPathEmpty :: Path base type' -> URI
mkUriPathEmpty path = URI Strict.Nothing
                     False
                     (URIAuth Strict.Nothing Localhost Strict.Nothing)
                     (V.fromList $ fmap T.pack $ splitOn "/" $ toFilePath path)
                     V.empty
                     Strict.Nothing

mkUriLoc :: URIAuth -> Location base type' -> URI
mkUriLoc auth loc = URI (Strict.Just "https")
                   True
                   auth
                   (V.fromList $ fmap T.pack $ splitOn "/" $ show loc)
                   ( V.fromList $ map (\(l,r) ->
                       (T.pack l) Strict.:!:
                            (maybe Strict.Nothing (Strict.Just . T.pack) r))
                       (getQuery loc)
                   )
                   (maybe Strict.Nothing (Strict.Just . T.pack) (getFragment loc))

mkUriLocEmpty :: Location base type' -> URI
mkUriLocEmpty loc = URI Strict.Nothing
                   False
                   (URIAuth Strict.Nothing Localhost Strict.Nothing)
                   (V.fromList $ fmap T.pack $ splitOn "/" $ show loc)
                   ( V.fromList $ map (\(l,r) ->
                       (T.pack l) Strict.:!:
                            (maybe Strict.Nothing (Strict.Just . T.pack) r))
                       (getQuery loc)
                   )
                   (maybe Strict.Nothing (Strict.Just . T.pack) (getFragment loc))
