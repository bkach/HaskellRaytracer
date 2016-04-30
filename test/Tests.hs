module Main where

import Test.Framework

import Data.Monoid

import Tests.Shapes (shapeTests)

main :: IO ()
main =
  defaultMainWithOpts
    [
      shapeTests
    ]
    mempty


