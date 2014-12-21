{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module MainSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import UrlPath

import Lucid
import Lucid.Base

import qualified Data.Text as T

import Data.Monoid


main :: IO ()
main = hspec spec

data Deploy = Rel
            | Gro
            | Abs

spec :: Spec
spec = do
  describe "deploying" deploy

deploy :: Spec
deploy =
 it "should intercalate `?` and `&`" $ property $
   testPath runRelativeUrl Rel
 it "should intercalate `?` and `&` and prepend `/`" $ property $
   testPath runGroundedUrl Gro
 it "should intercalate `?` and `&` and prepend the host" $ property $
   testPath runAbsoluteUrl Abs


testPath :: ( UrlReader T.Text m
            , Url T.Text m ) =>
            ( m T.Text T.Text
           -> T.Text
           -> T.Text ) -- runner
         -> Deploy -- deployment scheme
         -> T.Text -- Host
         -> T.Text -- Target
         -> [(T.Text, T.Text)] -- Keys and Values
         -> Property
testPath renderPath d host target kvs = do
  not (T.null host) ==>
    not (T.null target) ==>
      
              let host' = case d of
                            Rel -> ""
                            Gro -> "/"
                            Abs -> host <> "/"
              in

              runTest host'
  where
    runTest host' = do
      (renderPath (url $ target <?> (key1, val1) <&> (key2, val2)) host)
      === (host' <> target <> "?" <> key1 <> "=" <> val1 <> "&" <> key2 <> "=" <> val2)
