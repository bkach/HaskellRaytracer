module Main where

import Test.Framework

import Data.Monoid

import Tests.Shapes (shapeTests)
import Tests.Vector (vectorTests)

main :: IO ()
main =
  defaultMainWithOpts
    [
      shapeTests,
      vectorTests
    ]
    mempty


