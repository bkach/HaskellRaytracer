module Tests.Vector where

import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assertEqual)

import           Tests.Util

import           RayCaster.Vector

vectorTests :: Test
vectorTests =
  testGroup
    "Vector Tests"
    [ testProperty
        "Magnitude of vector is always positive"
        prop_vectorPositiveMagnitude
    , testProperty "Dot product is commutative" prop_dotProductCommutative
    ]

prop_vectorPositiveMagnitude :: Vector -> Bool
prop_vectorPositiveMagnitude vector = magnitude vector >= 0

prop_dotProductCommutative :: Vector -> Vector -> Bool
prop_dotProductCommutative vectorA vectorB =
  (vectorA `dot` vectorB) == (vectorB `dot` vectorA)
