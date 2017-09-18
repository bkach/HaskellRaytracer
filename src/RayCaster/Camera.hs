module RayCaster.Camera where

import           RayCaster.Vector

data Camera = Camera
  { fov            :: Double
  , cameraPosition :: Point
  , lookingAt      :: Vector
  }

eyeVector :: Camera -> Vector
eyeVector cam = normalize $ lookingAt cam `sub` cameraPosition cam
