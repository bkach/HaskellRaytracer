module Render where

import           Color        (Color (..), colorAdd, colorMult)
import           Data.Maybe   (Maybe, mapMaybe)
import           DataTypes    (Config (..), Light (..), Material (..),
                               Object (..), Scene (..))
import           Intersection (Intersection (..))
import           Ray          (Ray (..))
import qualified Ray
import           Shapes       (Shape)
import qualified Shapes
import           Vector       (Vector)
import qualified Vector

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
  Intersection obj <$> Shapes.rayIntersection ray s

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
        Vector.reflect (Shapes.normalAtPoint hitPoint shape) direction
      reflectionRay = Ray hitPoint reflectionDirection
  in if reflections == maxReflections
       then Color 0 0 0
       else traceRayReflect scene reflectionRay (reflections + 1)

isLightVisible :: [Object] -> Vector -> Light -> Bool
isLightVisible objects point light =
  let toLightVector = center light `Vector.sub` point
      distanceToLight = Vector.magnitude toLightVector
      direction = Vector.normalize toLightVector
      ray = Ray point direction
      shapes = map (\(Object shape _) -> shape) objects
      objIntersections = mapMaybe (Shapes.rayIntersection ray) shapes
  in all (>= distanceToLight) objIntersections

-- Should also include light color
lambertColor :: Vector -> Object -> [Light] -> Color
lambertColor hitPoint (Object shape (Material color)) lights =
  let normal = Shapes.normalAtPoint hitPoint shape
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
  let lightDirection = Vector.normalize $ center light `Vector.sub` point
  in max 0 (normal `Vector.dot` lightDirection)
