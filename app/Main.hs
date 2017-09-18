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

import           Camera
import           Codec.Picture
import           Codec.Picture.Png
import           Color
import           Data.List
import           Data.Maybe
import           DataTypes
import           Debug.Trace
import           Intersection
import           Quaternion
import           Ray               (Ray (..))
import qualified Ray
import           Shapes
import           Transformations
import           Vector

main :: IO ()
main =
  let objects =
        [ Object (Sphere (Vector (-0.5) 0 2) 0.5) (Material Color.red)
        , Object (Sphere (Vector 0 0 3) 0.5) (Material Color.blue)
        , Object
            (Plane (Vector 0 (-0.5) 0) (Vector 0 1 0))
            (Material Color.pink)
        , Object (Plane (Vector 0 0 5) (Vector 0 0 (-1))) (Material Color.pink)
        ]
      lights =
        [ PointLight (Vector 0 0.5 0) 0.4
        , PointLight (Vector 0.5 0.5 0) 0.4
        , PointLight (Vector 9 0 4) 0.2
        ]
      camera =
        rotateCamera
          (Vector 0 0 3)
          (Vector 0 1 0)
          (-90)
          (Camera 45 (Vector 0 0 (-1)) (Vector 0 0 3))
      config = Config 500 500 Color.black
      scene = Scene objects lights camera config
      width = sceneWidth config
      height = sceneHeight config
      img =
        generateImage
          (\x y -> pixelRGB8 $ getCoordColor scene x (height - y))
          width
          height
  in writePng "output.png" img

getCoordColor :: Scene -> Int -> Int -> Color
getCoordColor scene@(Scene _ _ camera config) x y =
  let ray = Ray.generate camera (sceneWidth config) (sceneHeight config) x y
  in traceRay scene ray

traceRay :: Scene -> Ray -> Color
traceRay scene ray = traceRayReflect scene ray 0

traceRayReflect :: Scene -> Ray -> Int -> Color
traceRayReflect scene@(Scene objects _ _ config) ray reflections =
  let bgColor = defaultColor config
      intersectedObject = closestObject ray objects
  in maybe
       bgColor
       (getIntersectionColor ray scene reflections)
       intersectedObject

closestObject :: Ray -> [Object] -> Maybe Intersection
closestObject ray objects =
  let intersections = mapMaybe (objectIntersection ray) objects
  in if null intersections
       then Nothing
       else Just $ minimum intersections

objectIntersection :: Ray -> Object -> Maybe Intersection
objectIntersection ray obj@(Object s _) =
  Intersection obj <$> rayIntersection ray s

getIntersectionColor :: Ray -> Scene -> Int -> Intersection -> Color
getIntersectionColor ray scene@(Scene objects lights _ _) reflections (Intersection hitObject hitDistance) =
  let hitPoint = Ray.pointAlongRay ray hitDistance
      otherObjects = filter (/= hitObject) objects
      visibleLights = filter (isLightVisible otherObjects hitPoint) lights
      lambertVal = lambertColor hitPoint hitObject visibleLights
      reflectionLight = reflectionColor scene ray hitPoint hitObject reflections
  in lambertVal `colorAdd` (0.2 `colorMult` reflectionLight)

reflectionColor :: Scene -> Ray -> Vector -> Object -> Int -> Color
reflectionColor scene (Ray origin direction) hitPoint (Object shape _) reflections =
  let maxReflections = 2
      reflectionDirection =
        Vector.reflect (normalAtPoint hitPoint shape) direction
      reflectionRay = Ray hitPoint reflectionDirection
  in if reflections == maxReflections
       then Color 0 0 0
       else traceRayReflect scene reflectionRay (reflections + 1)

isLightVisible :: [Object] -> Vector -> Light -> Bool
isLightVisible objects point light =
  let toLightVector = center light `sub` point
      distanceToLight = magnitude toLightVector
      direction = normalize toLightVector
      ray = Ray point direction
      shapes = map (\(Object shape _) -> shape) objects
      objIntersections = mapMaybe (rayIntersection ray) shapes
  in all (>= distanceToLight) objIntersections

-- Should also include light color
lambertColor :: Vector -> Object -> [Light] -> Color
lambertColor hitPoint (Object shape (Material color)) lights =
  let normal = normalAtPoint hitPoint shape
      lightIlluminations = fmap (lambertIllumination hitPoint normal) lights
      totalIllumination = sum lightIlluminations
  in totalIllumination `colorMult` color

lambertIllumination :: Vector -> Vector -> Light -> Double
lambertIllumination hitPoint normal light =
  let lv = lambertValue hitPoint normal light
      illumination = lv * intensity light
    -- When lights have colors, they'll be multiplied here
  in illumination

lambertValue :: Vector -> Vector -> Light -> Double
lambertValue point normal light =
  let lightDirection = normalize $ center light `sub` point
  in max 0 (normal `dot` lightDirection)
