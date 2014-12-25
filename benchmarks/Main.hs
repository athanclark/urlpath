module Main where

import Criterion.Main
import qualified Data.Text as T
import TextBenchmarks (TextBenchmark (..), benchmarks)

-- | Function to run the benchmarks using criterion
main :: IO ()
main = defaultMain $ map benchUrl benchmarks
  where
    benchUrl (TextBenchmark name f x) = bgroup name
      [bench "Text" $ nf (T.length . f) x
      ]
