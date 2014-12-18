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

data Deploy = Rel
            | Gro
            | Abs

spec :: Spec
spec = do
  describe "rendering" $ do
    it "should intercalate `?` and `&`" $ property $
      testPath runRelativeUrl Rel
    it "should intercalate `?` and `&` and prepend `/`" $ property $
      testPath runGroundedUrl Gro
    it "should intercalate `?` and `&` and prepend the host" $ property $
      testPath runAbsoluteUrl Abs
  where
    testPath :: ( UrlReader T.Text m
                , Url T.Text m ) =>
                ( m T.Text
               -> T.Text
               -> T.Text ) -- runner
             -> Deploy -- deployment scheme
             -> T.Text -- Host
             -> T.Text -- Target
             -> T.Text -- Key1
             -> T.Text -- Val1
             -> T.Text -- Key2
             -> T.Text -- Val2
             -> Property
    testPath renderPath d host target key1 val1 key2 val2 = do
      let host' = case d of
                    Rel -> ""
                    Gro -> "/"
                    Abs -> host <> "/"

      not (T.null host) ==>
        not (T.null target) ==>
          not (T.null key1) ==>
            not (T.null val1) ==>
              not (T.null key2) ==>
                not (T.null val2) ==>
                  (renderPath (url $ target <?> (key1, val1) <&> (key2, val2)) host)
                  === (host' <> target <> "?" <> key1 <> "=" <> val1 <> "&" <> key2 <> "=" <> val2)
