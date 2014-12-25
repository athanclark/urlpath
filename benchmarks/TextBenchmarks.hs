{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TextBenchmarks where

import UrlPath

import qualified Data.Text as T

import Data.String
import Data.Monoid


data TextBenchmark = forall a. TextBenchmark
  String        -- ^ Name.
  (a -> T.Text) -- ^ Rendering function.
  a             -- ^ Data.

-- | List containing all benchmarks.
benchmarks :: [TextBenchmark]
benchmarks =
  [ TextBenchmark "foo" expandRelative $
      "foo" <?> ("bar","baz") <&> ("key","val")
  ]
