-- Copyright 2016 Boris Kachscovsky
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
module Main where

import DataTypes
import Shapes
import Ray
import Vector
import Quaternion
import Color
import Camera
import Transformations
import Utils (degreesToRadians)
import Codec.Picture
import Codec.Picture.Png
import Data.Maybe
import Data.List
import Debug.Trace

main :: IO()
main =
  let
    objects :: [Object]
    objects = [Object
                    (Sphere (Vector (-0.5) 0 2) 0.5)
                    (Material Color.red),
               Object
                    (Sphere (Vector 0 0 3) 0.5)
                    (Material Color.blue),
               Object
                    (Plane (Vector 0 (-0.5) 0) (Vector 0 1 0))
                    (Material Color.pink),
               Object
                    (Plane (Vector 0 0 5) (Vector 0 0 (-1)))
                    (Material Color.pink)
              ]

    lights :: [Light]
    lights = [PointLight (Vector 0 0.5 0) 0.4, PointLight (Vector 0.5 0.5 0) 0.4, PointLight (Vector 9 0 4) 0.2]

    camera :: Camera
    camera = rotateCamera (Vector 0 0 3) (Vector 0 1 0) (-90) (Camera 45 (Vector 0 0 (-1)) (Vector 0 0 3))

    config = Config 500 500 Color.white

    scene :: Scene
    scene = Scene objects lights camera config

    img = generateImage (\x y -> pixelRGB8 $ Main.trace scene (sceneWidth config - x) (sceneHeight config - y)) (sceneWidth config) (sceneHeight config)
   in
    writePng "output.png" img

trace :: Scene -> Int -> Int -> Color
trace (Scene objects lights camera config) x y =
    let
      backgroundColor = defaultColor config
      ray =  generateRay camera (sceneWidth config) (sceneHeight config) x y
      maybeIntersectedObject = closestObject ray objects
    in
        case maybeIntersectedObject of
            Nothing -> backgroundColor
            Just intersectionObj  ->
                getColorFromIntersection backgroundColor ray lights objects intersectionObj


getColorFromIntersection :: Color -> Ray -> [Light] -> [Object] -> (Double, Object) -> Color
getColorFromIntersection defaultColor ray lights objects (hitDistance, hitObject) =
    let
        hitPoint = pointAlongRay ray hitDistance
        otherObjects = filter (/= hitObject) objects
        visibleLights = filter (isLightVisible otherObjects hitPoint) lights
    in
        lambertColor hitPoint hitObject visibleLights

isLightVisible :: [Object] -> Vector -> Light -> Bool
isLightVisible objects point light =
    let
        toLightVector = center light `sub` point
        distanceToLight = magnitude toLightVector
        direction = normalize toLightVector
        ray = Ray point direction
        shapes = map (\(Object shape _) -> shape) objects
        objIntersections = mapMaybe (rayIntersection ray) shapes
    in
        all (>= distanceToLight) objIntersections

-- Should also include light color
lambertColor :: Vector -> Object -> [Light] -> Color
lambertColor hitPoint (Object shape (Material color)) lights =
    let
        normal = normalAtPoint hitPoint shape
        lightLamberts = fmap (\l -> (lambertIntensity hitPoint normal l, l)) lights
        lIntensity = sum $ fmap fst lightLamberts
    in lIntensity `scalarMult` color

lambertIntensity :: Vector -> Vector -> Light -> Double
lambertIntensity point normal (PointLight center intensity) =
    let lightDirection = normalize $ center `sub` point
    in intensity * max 0 (normal `dot` lightDirection)


-- Generating rays, assuming distance to the image is 1 unit
generateRay :: Camera -> Int -> Int -> Int -> Int -> Ray
generateRay camera width height x y =
    let
         -- Vector from camera to lookAt point
        centerVector = eyeVector camera
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
        scaledX = ((fromIntegral x * pixelWidth) - halfWidth)  `scalarMult` rightVector
        scaledY = ((fromIntegral y * pixelHeight) - halfHeight) `scalarMult`  upVector
        orientation = normalize $ centerVector `add` scaledX `add` scaledY
    in  Ray (cameraPosition camera) orientation
    where w = fromIntegral width
          h = fromIntegral height

closestObject :: Ray -> [Object] -> Maybe (Double, Object)
closestObject ray objects =
  let
    intersections = mapMaybe intersectLambda objects
  in
    if null intersections then
      Nothing
    else
      Just $ minimumBy minimumDefinedByFirst intersections
  where
    intersectLambda :: Object -> Maybe (Double, Object)
    intersectLambda obj@(Object s _) = fmap (\i -> (i, obj)) (rayIntersection ray s)

minimumDefinedByFirst :: (Double, Object) -> (Double,Object) -> Ordering
minimumDefinedByFirst  x y
    | fst x < fst y = LT
    | fst x > fst y = GT
    | otherwise = EQ

