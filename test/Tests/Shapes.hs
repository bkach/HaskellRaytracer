module Tests.Shapes where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Shapes
import Ray
import Vector

import Data.Maybe (isJust)

shapeTests :: Test
shapeTests =
  testGroup "Shape Tests" [
    testCase "Check sphere intersection works" test_sphereIntersection,
    testCase "Check plane intersection works" test_planeIntersection,
    testProperty "QuickCheck test for intersection" prop_rayIntersectonWorks
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

test_planeIntersection :: Assertion
test_planeIntersection =
  let
    plane = Plane (Vector 0 0 0) (Vector 0 1 0)
    ray = Ray (Vector 0 10 0) (Vector 0 (-1) 0)
    expected = Just 10 
    distance = rayIntersection ray plane
  in
    assertEqual "" distance expected

prop_rayIntersectonWorks :: (Double, Double) -> Bool
prop_rayIntersectonWorks (xVal, zVal) =
  let
    plane = Plane (Vector 0 0 0) (Vector 0 1 0)
    ray = Ray (Vector xVal 10 zVal) (Vector 0 (-1) 0)
  in
    isJust $ rayIntersection ray plane


