{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module PathExamples where

import Lucid
import Lucid.Base
import Lucid.Path
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Functor.Identity
import Control.Monad.Reader

example :: UrlReader f => HtmlT f () -> LT.Text
example x = (runUrlReader $ renderTextT x) "example.com"

template :: Url UrlString f => HtmlT f ()
template = do
  url <- lift $ renderUrl $ "foo.php" <?> ("key","bar")
  script_ [src_ url] ""

relative :: HtmlT RelativeUrl ()
relative = template

grounded :: HtmlT GroundedUrl ()
grounded = template

absolute :: HtmlT AbsoluteUrl ()
absolute = template

relative' :: LT.Text
relative' = example relative

grounded' :: LT.Text
grounded' = example grounded

absolute' :: LT.Text
absolute' = example absolute

-- Monad transformers

relativeT :: Monad m => HtmlT (RelativeUrlT m) ()
relativeT = template

groundedT :: Monad m => HtmlT (GroundedUrlT m) ()
groundedT = template

absoluteT :: Monad m => HtmlT (AbsoluteUrlT m) ()
absoluteT = template

exampleT :: (MonadReader T.Text (f m), Monad m) =>
            HtmlT (f m) ()
         -> (f m LT.Text -> T.Text -> m LT.Text)
         -> m LT.Text
exampleT x f = (f $ renderTextT x) "example.com"

relativeT' :: LT.Text
relativeT' = runIdentity $ exampleT relativeT runRelativeUrlT

groundedT' :: LT.Text
groundedT' = runIdentity $ exampleT groundedT runGroundedUrlT

absoluteT' :: LT.Text
absoluteT' = runIdentity $ exampleT absoluteT runAbsoluteUrlT

-- Plain text

plain :: HtmlT Identity ()
plain = do
  url <- lift $ renderUrl $ ("foo.php" :: T.Text)
  script_ [src_ url] ""

plain' :: LT.Text
plain' = renderText plain
