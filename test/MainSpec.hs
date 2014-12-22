{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module MainSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import UrlPath

import qualified Data.Text as T

import Data.String
import Data.Monoid
import Data.Functor.Identity

main :: IO ()
main = hspec spec

data Deploy = Rel
            | Gro
            | Abs

spec :: Spec
spec = do
  describe "deploying" deploy

deploy :: Spec
deploy = do
 it "should intercalate `?` and `&`" ( property $
   testPath (\m h -> runIdentity $ runRelativeUrlT m h) Rel
   )
 it "should intercalate `?` and `&` and prepend `/`" ( property $
   testPath (\m h -> runIdentity $ runGroundedUrlT m h) Gro
   )
 it "should intercalate `?` and `&` and prepend the host" $ property $
   testPath (\m h -> runIdentity $ runAbsoluteUrlT m h) Abs


-- | Render arbitrary Url combinations, with an abstracted deployment method
testPath :: ( UrlReader T.Text m
            , Url T.Text m ) =>
            ( m T.Text
           -> T.Text
           -> T.Text ) -- runner
         -> Deploy -- deployment scheme
         -> T.Text -- Host
         -> T.Text -- Target
         -> [(T.Text, T.Text)] -- Keys and Values
         -> Property
testPath pathRunner d host target keyval = do
  let host' = case d of
                Rel -> ""
                Gro -> "/"
                Abs -> host <> "/"

  (pathRunner (url $ urlStringTail target keyval) host)
    === (rawRender host' target keyval)

  where
    urlStringTail :: ( IsString a
                     , Monoid a
                     ) => a -> [(a,a)] -> UrlString a

    urlStringTail t [] = UrlString t []
    urlStringTail t [kv] = t <?> kv
    urlStringTail t (kv:kvs) =
      foldl (<&>) (t <?> kv) kvs


-- | Manual Url rendering
rawRender :: ( IsString a
             , Monoid a
             ) => a -> a -> [(a,a)] -> a
rawRender rawHost target kvs =
  rawHost <> target <> rawUrlTail kvs


-- | Manually intercalate characters
rawUrlTail :: ( IsString a
              , Monoid a
              ) => [(a,a)] -> a
rawUrlTail [] = ""
rawUrlTail [(k,v)] = "?" <> k <> "=" <> v
rawUrlTail ((k,v):kvs) =
  foldl urlTail ("?" <> k <> "=" <> v) kvs
  where
    urlTail acc (x,y) = acc <> "&" <> x <> "=" <> y
