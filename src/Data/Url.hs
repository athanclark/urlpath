{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , KindSignatures
  , FlexibleInstances
  , TypeSynonymInstances
  , UndecidableInstances
  , MultiParamTypeClasses
  #-}

module Data.Url where

import Path.Extended

import Data.Functor.Identity
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


-- | Make an instance for your own stringless route type to use your symbols
-- instead of strings or @Path@.
class ToLocation sym base type' where
  toLocation :: MonadThrow m => sym -> m (Location base type')


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
      sh ++ ":"
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
  { runRelativeUrlT :: UrlAuthority -> m a
  } deriving Functor

type RelativeUrl = RelativeUrlT Identity

instance ( Applicative m
         ) => MonadUrl Rel File (RelativeUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)

instance ( Applicative m
         ) => MonadUrl Rel Dir (RelativeUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)

instance Applicative m => Applicative (RelativeUrlT m) where
  pure x = RelativeUrlT $ const (pure x)
  f <*> x = RelativeUrlT $ \r ->
    runRelativeUrlT f r <*> runRelativeUrlT x r

instance Monad m => Monad (RelativeUrlT m) where
  return x = RelativeUrlT $ const (return x)
  m >>= f = RelativeUrlT $ \r ->
    runRelativeUrlT m r >>= (\x -> runRelativeUrlT (f x) r)

instance MonadTrans RelativeUrlT where
  lift = RelativeUrlT . const

instance MonadIO m => MonadIO (RelativeUrlT m) where
  liftIO = lift . liftIO

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

instance ( MonadRWS r w s m
         ) => MonadRWS r w s (RelativeUrlT m) where

instance ( MonadCont m
         ) => MonadCont (RelativeUrlT m) where
  callCC f = RelativeUrlT $ \r ->
    callCC $ \c -> runRelativeUrlT (f (RelativeUrlT . const . c)) r

instance ( MonadError e m
         ) => MonadError e (RelativeUrlT m) where
  throwError = lift . throwError
  catchError (RelativeUrlT x) f = RelativeUrlT $ \r ->
    catchError (x r) (flip runRelativeUrlT r . f)

instance ( MonadBase b m
         ) => MonadBase b (RelativeUrlT m) where
  liftBase = liftBaseDefault

instance MonadTransControl RelativeUrlT where
  type StT RelativeUrlT a = a
  liftWith f = RelativeUrlT $ \r ->
    f $ \t -> runRelativeUrlT t r
  restoreT = RelativeUrlT . const

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (RelativeUrlT m) where
  type StM (RelativeUrlT m) a = ComposeSt RelativeUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance ( MonadThrow m
         ) => MonadThrow (RelativeUrlT m) where
  throwM = lift . throwM

instance ( MonadCatch m
         ) => MonadCatch (RelativeUrlT m) where
  catch (RelativeUrlT x) f = RelativeUrlT $ \r ->
    catch (x r) (flip runRelativeUrlT r . f)

instance ( MonadMask m
         ) => MonadMask (RelativeUrlT m) where
  mask a = RelativeUrlT $ \r ->
    mask $ \u -> runRelativeUrlT (a $ q u) r
    where q u (RelativeUrlT x) = RelativeUrlT (u . x)
  uninterruptibleMask a = RelativeUrlT $ \r ->
    uninterruptibleMask $ \u -> runRelativeUrlT (a $ q u) r
    where q u (RelativeUrlT x) = RelativeUrlT (u . x)

instance ( MonadLogger m
         ) => MonadLogger (RelativeUrlT m) where
  monadLoggerLog a b c d = lift (monadLoggerLog a b c d)

instance ( MonadResource m
         ) => MonadResource (RelativeUrlT m) where
  liftResourceT = lift . liftResourceT

instance MFunctor RelativeUrlT where
  hoist f (RelativeUrlT x) = RelativeUrlT $ \r ->
    f (x r)

instance MMonad RelativeUrlT where
  embed f x = RelativeUrlT $ \r ->
    runRelativeUrlT (f (runRelativeUrlT x r)) r


-- ** Grounded Urls

newtype GroundedUrlT m a = GroundedUrlT
  { runGroundedUrlT :: UrlAuthority -> m a
  } deriving Functor

type GroundedUrl = GroundedUrlT Identity

instance ( Applicative m
         ) => MonadUrl Abs File (GroundedUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)

instance ( Applicative m
         ) => MonadUrl Abs Dir (GroundedUrlT m) where
  pathUrl x   = pure (toFilePath x)
  locUrl x    = pure (show x)

instance Applicative m => Applicative (GroundedUrlT m) where
  pure x = GroundedUrlT $ const (pure x)
  f <*> x = GroundedUrlT $ \r ->
    runGroundedUrlT f r <*> runGroundedUrlT x r

instance Monad m => Monad (GroundedUrlT m) where
  return x = GroundedUrlT $ const (return x)
  m >>= f = GroundedUrlT $ \r ->
    runGroundedUrlT m r >>= (\x -> runGroundedUrlT (f x) r)

instance MonadTrans GroundedUrlT where
  lift = GroundedUrlT . const

instance MonadIO m => MonadIO (GroundedUrlT m) where
  liftIO = lift . liftIO

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

instance ( MonadRWS r w s m
         ) => MonadRWS r w s (GroundedUrlT m) where

instance ( MonadCont m
         ) => MonadCont (GroundedUrlT m) where
  callCC f = GroundedUrlT $ \r ->
    callCC $ \c -> runGroundedUrlT (f (GroundedUrlT . const . c)) r

instance ( MonadError e m
         ) => MonadError e (GroundedUrlT m) where
  throwError = lift . throwError
  catchError (GroundedUrlT x) f = GroundedUrlT $ \r ->
    catchError (x r) (flip runGroundedUrlT r . f)

instance ( MonadBase b m
         ) => MonadBase b (GroundedUrlT m) where
  liftBase = liftBaseDefault

instance MonadTransControl GroundedUrlT where
  type StT GroundedUrlT a = a
  liftWith f = GroundedUrlT $ \r ->
    f $ \t -> runGroundedUrlT t r
  restoreT = GroundedUrlT . const

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (GroundedUrlT m) where
  type StM (GroundedUrlT m) a = ComposeSt GroundedUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance ( MonadThrow m
         ) => MonadThrow (GroundedUrlT m) where
  throwM = lift . throwM

instance ( MonadCatch m
         ) => MonadCatch (GroundedUrlT m) where
  catch (GroundedUrlT x) f = GroundedUrlT $ \r ->
    catch (x r) (flip runGroundedUrlT r . f)

instance ( MonadMask m
         ) => MonadMask (GroundedUrlT m) where
  mask a = GroundedUrlT $ \r ->
    mask $ \u -> runGroundedUrlT (a $ q u) r
    where q u (GroundedUrlT x) = GroundedUrlT (u . x)
  uninterruptibleMask a = GroundedUrlT $ \r ->
    uninterruptibleMask $ \u -> runGroundedUrlT (a $ q u) r
    where q u (GroundedUrlT x) = GroundedUrlT (u . x)

instance ( MonadLogger m
         ) => MonadLogger (GroundedUrlT m) where
  monadLoggerLog a b c d = lift (monadLoggerLog a b c d)

instance ( MonadResource m
         ) => MonadResource (GroundedUrlT m) where
  liftResourceT = lift . liftResourceT

instance MFunctor GroundedUrlT where
  hoist f (GroundedUrlT x) = GroundedUrlT $ \r ->
    f (x r)

instance MMonad GroundedUrlT where
  embed f x = GroundedUrlT $ \r ->
    runGroundedUrlT (f (runGroundedUrlT x r)) r


-- ** Absolute Urls

newtype AbsoluteUrlT m a = AbsoluteUrlT
  { runAbsoluteUrlT :: UrlAuthority -> m a
  } deriving Functor

type AbsoluteUrl = AbsoluteUrlT Identity

instance ( Applicative m
         ) => MonadUrl Abs File (AbsoluteUrlT m) where
  pathUrl x   = AbsoluteUrlT (\h -> pure $ showUrlAuthority h ++ toFilePath x)
  locUrl x    = AbsoluteUrlT (\h -> pure $ showUrlAuthority h ++ show x)

instance Applicative m => Applicative (AbsoluteUrlT m) where
  pure x = AbsoluteUrlT $ const (pure x)
  f <*> x = AbsoluteUrlT $ \r ->
    runAbsoluteUrlT f r <*> runAbsoluteUrlT x r

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
