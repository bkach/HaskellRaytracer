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

import Vector
import Color
import Camera
import Utils (degreesToRadians, roots)
import Codec.Picture
import Codec.Picture.Png
import Data.Maybe
import Data.List
import Debug.Trace

-- Basic Data Types
data Object = Object Shape Material deriving(Eq)
data Material = Material Color deriving(Eq)
data Shape = Sphere Vector Double  -- center, radius
           | Plane Vector Vector  deriving(Eq) -- center, normal
data Light = PointLight {center :: Vector, intensity :: Double} -- center, intensity
data Scene = Scene [Object] [Light] Camera Config
data Config = Config { sceneWidth :: Int,
                       sceneHeight :: Int,
                       defaultColor :: Color }

data Ray = Ray {origin :: Vector, direction :: Vector}

main :: IO()
main =
  let
    objects :: [Object]
    objects = [Object
                    (Sphere (Vector 0.5 0 2) 0.5)
                    (Material Color.red),
               Object
                    (Sphere (Vector (-1) 0 4) 0.5)
                    (Material Color.green),
               Object
                    (Sphere (Vector 0 0 3) 0.5)
                    (Material Color.blue),
               Object
                    (Plane (Vector 0 (-1) 0) (Vector 0 1 0))
                    (Material Color.pink),
               Object
                    (Plane (Vector 0 0 4.5) (Vector 0 0 (-1)))
                    (Material Color.pink)]

    lights :: [Light]
    lights = [PointLight (Vector 0 0.5 0) 0.8, PointLight (Vector 0.5 0.5 0) 0.2]

    camera :: Camera
    camera = Camera 45 (Vector 0 0 0) (Vector 0 0 1)

    config = Config 500 500 Color.white

    scene :: Scene
    scene = Scene objects lights camera config

    img = generateImage (\x y -> pixelRGB8 $ Main.trace scene x (sceneHeight config - y)) (sceneWidth config) (sceneHeight config)
   in
    writePng "output.png" img

trace :: Scene -> Int -> Int -> Color
trace (Scene objects lights camera config) x y =
    let
      backgroundColor = defaultColor config
      ray =  generateRay camera (sceneWidth config) (sceneHeight config) x y
      intersection = closestIntersection ray objects
    in
        case intersection of 
            Nothing -> backgroundColor
            (Just intersectionObj@(direction,object)) -> 
                getColorFromIntersection object backgroundColor ray lights objects intersectionObj

isLightVisible :: [Object] -> Vector -> Light -> Bool
isLightVisible objects point light = isLightVisible' objects point light True

isLightVisible' [] point light acc = acc
isLightVisible' (object@(Object shape material):objects) point light@(PointLight center _) True = 
    let 
        direction = normalize $ center `sub` point
        intersection = minIntersection (Ray point direction) object
        recursiveCall = isLightVisible' objects point light
    in
        maybe (recursiveCall True) (\(distance,_) -> (distance < 0)) intersection

getColorFromIntersection :: Object -> Color -> Ray -> [Light] -> [Object] -> (Double, Object) -> Color
getColorFromIntersection currentObject defaultColor ray lights objects (hitDistance , Object shape (Material color)) = 
    let 
        hitPoint = pointAlongRay ray hitDistance 
        otherObjects = filter (/= currentObject) objects 
        pointHitsLight = isLightVisible otherObjects hitPoint
        dimmedLights = map 
                (\light -> 
                    if pointHitsLight light 
                    then light 
                    else PointLight (center light) (0.15 * intensity light)) lights
    in
        lambertColor hitPoint color shape dimmedLights 

-- Should also include light color
lambertColor :: Vector -> Color -> Shape -> [Light] -> Color
lambertColor hitPoint color shape lights = 
    let normal = normalAtPoint hitPoint shape
        lIntensity = totalLambertIntensity hitPoint normal lights
    in lIntensity `scalarMult` color

pointAlongRay :: Ray -> Double -> Vector
pointAlongRay ray distance = origin ray `add` (distance `scalarMult` direction ray)

normalAtPoint :: Vector -> Shape -> Vector
normalAtPoint point (Sphere center radius) = normalize (point `sub` center)
normalAtPoint point (Plane center normal) = normal

totalLambertIntensity :: Vector -> Vector -> [Light] -> Double
totalLambertIntensity point normal lights =
    sum $ map (lambertIntensity point normal) lights

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

closestIntersection :: Ray -> [Object] -> Maybe (Double, Object)
closestIntersection ray objects
    | null intersections = Nothing
    | otherwise = Just $ minimumBy minimumDefinedByFirst intersections
    where intersections = mapMaybe (minIntersection ray) objects

minimumDefinedByFirst :: (Double, Object) -> (Double,Object) -> Ordering
minimumDefinedByFirst  x y
    | fst x < fst y = LT
    | fst x > fst y = GT
    | otherwise = EQ

-- Minimum distance intersection
minIntersection :: Ray -> Object -> Maybe (Double, Object)
minIntersection (Ray origin direction) object@(Object (Sphere center radius) _) =
    let
        l = origin `sub` center
        a = direction `dot` direction
        b = 2 * (direction `dot` l)
        c =  (l `dot` l) - radius^2
        listOfRoots = roots a b c
    in
        case listOfRoots of
            [] -> Nothing
            otherwise -> Just (minimum listOfRoots, object)
minIntersection (Ray origin direction) object@(Object (Plane center normal) _) =
    let distance = ((center `sub` origin) `dot` normal) / (direction `dot` normal)
    in if distance < 0 then Nothing else Just (distance, object)
