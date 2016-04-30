module Tests.Shapes where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Shapes
import Ray
import Vector

shapeTests :: Test
shapeTests =
  testGroup "Shape Tests" [
    testCase "Check sphere intersection works" test_sphereIntersection
  ]

test_sphereIntersection :: Assertion
test_sphereIntersection =
  let
    sphere = Sphere (Vector 0 0 0) 1
    ray = Ray (Vector 0 0 10) (Vector 0 0 (-1))
    expected = Just 9
    distance = rayIntersection ray sphere
  in
    assertEqual "" distance expected

