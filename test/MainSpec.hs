{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module MainSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import UrlPath

import qualified Data.Text as T

import Data.Monoid


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rendering" $ do
    it "should intercalate `?` and `&`" $ property $
      testPath runRelativeUrl
  where
    testPath :: ( UrlReader m
                , Url UrlString m ) =>
                ( m T.Text
               -> T.Text
               -> T.Text )
             -> T.Text -- Host
             -> T.Text -- Target
             -> T.Text -- Key1
             -> T.Text -- Val1
             -> T.Text -- Key2
             -> T.Text -- Val2
             -> Property
    testPath renderPath host target key1 val1 key2 val2 =
      not (T.null host) ==>
      not (T.null target) ==>
      not (T.null key1) ==>
      not (T.null val1) ==>
      not (T.null key2) ==>
      not (T.null val2) ==>
        (renderPath (renderUrl $ target <?> (key1, val1) <&> (key2, val2)) host)
        === (target <> "?" <> key1 <> "=" <> val1 <> "&" <> key2 <> "=" <> val2)
