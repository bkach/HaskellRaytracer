module Tests.Vector where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, arbitrary)

import Vector

instance Arbitrary Vector where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Vector x y z

vectorTests :: Test
vectorTests =
  testGroup "Vector Tests" [
    testProperty "Magnitude of vector is always positive" prop_vectorPositiveMagnitude,
    testProperty "Dot product is commutative" prop_dotProductCommutative
  ]

prop_vectorPositiveMagnitude :: Vector -> Bool
prop_vectorPositiveMagnitude vector =
  magnitude vector >= 0

prop_dotProductCommutative :: Vector -> Vector -> Bool
prop_dotProductCommutative vectorA vectorB =
  (vectorA `dot` vectorB) == (vectorB `dot` vectorA)

