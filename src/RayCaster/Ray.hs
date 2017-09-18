module RayCaster.Ray where

import           RayCaster.Camera
import           RayCaster.Utils  (degreesToRadians)
import           RayCaster.Vector

data Ray = Ray
  { origin    :: Vector
  , direction :: Vector
  }

pointAlongRay :: Ray -> Double -> Vector
pointAlongRay ray distance =
  origin ray `add` (distance `scalarMult` direction ray)

-- Generating rays, assuming distance to the image is 1 unit
generate :: Camera -> Int -> Int -> Int -> Int -> Ray
generate camera width height x y
     -- Vector from camera to lookAt point
 =
  let centerVector = eyeVector camera
    -- Vector in local right direction
      rightVector = normalize (centerVector `cross` Vector 0 1 0)
      upVector = normalize (rightVector `cross` centerVector)
    -- Halves are taken to make right angles
      halfFov = fov camera / 2
    -- This aspect ratio will be used, but are not the width and height of the camera
      aspectRatio = h / w
      halfWidth = tan $ degreesToRadians halfFov
      halfHeight = aspectRatio * halfWidth
      cameraWidth = halfWidth * 2
      cameraHeight = halfHeight * 2
      pixelWidth = cameraWidth / (w - 1)
      pixelHeight = cameraHeight / (h - 1)
      scaledX =
        ((fromIntegral x * pixelWidth) - halfWidth) `scalarMult` rightVector
      scaledY =
        ((fromIntegral y * pixelHeight) - halfHeight) `scalarMult` upVector
      orientation = normalize $ centerVector `add` scaledX `add` scaledY
  in Ray (cameraPosition camera) orientation
  where
    w = fromIntegral width
    h = fromIntegral height
