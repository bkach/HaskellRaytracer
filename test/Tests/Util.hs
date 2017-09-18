module Tests.Util where

import           Test.QuickCheck  (Arbitrary, arbitrary)

import           RayCaster.Camera
import           RayCaster.Vector

instance Arbitrary Vector where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Vector x y z

instance Arbitrary Camera where
  arbitrary = do
    fov <- arbitrary
    pos <- arbitrary
    look <- arbitrary
    return $ Camera fov pos look
