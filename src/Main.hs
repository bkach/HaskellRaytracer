module Main where

import Vector
import Quaternion
import Color
--import Camera
import Utils (degreesToRadians, roots)
import Codec.Picture
import Codec.Picture.Png
import Data.Maybe
import Data.List
import Debug.Trace

-- Basic Data Types
data Container = Container Vector Object deriving(Eq) -- center
data Object = Shape Geometry Material
            | Light LightType 
            | Camera Double Vector deriving(Eq) -- fov, lookAtVector
data LightType = PointLight Double deriving(Eq) -- intensity
data Material = Material Color deriving(Eq)
data Geometry = Sphere Double -- radius
           | Plane Vector  deriving(Eq) -- normal
data Scene = Scene [Container] Config
data Config = Config { sceneWidth :: Int, sceneHeight :: Int, defaultColor :: Color }
data Ray = Ray {origin :: Vector, direction :: Vector}

main :: IO()
main =
  let

    center = Vector 0 0 5

    containers :: [Container]
    containers =
             [Container center (Shape (Sphere 0.5) (Material Color.red)),
              rotateAroundPoint center (Vector 0 0 1) 45 (Container (Vector 1 0 5) (Shape (Sphere 0.5) (Material Color.blue))),
              Container (Vector 0 (-1) 5) (Shape (Sphere 0.5) (Material Color.green)),
              Container (Vector 0 (-1) 5) (Shape (Sphere 0.5) (Material Color.green)),
              Container (Vector 0 (-2) 0) (Shape (Plane (Vector 0 1 0)) (Material Color.green)),
              Container (Vector 0 0 7) (Shape (Plane (Vector 0 0 (-1))) (Material Color.red)),
              Container (Vector 0 0.8 0) (Light (PointLight 0.8)),
              Container (Vector 0.5 0.5 0) (Light (PointLight 0.1)),
              Container (Vector 0 0 (-1)) (Camera 45 (Vector 0 0 1))]

    config = Config 500 500 Color.white

    scene :: Scene
    scene = Scene containers config

    img = generateImage (\x y -> pixelRGB8 $ raytrace scene x (sceneHeight config - y)) (sceneWidth config) (sceneHeight config)
   in
     writePng "output.png" img

isCamera :: Container -> Bool
isCamera (Container _ (Camera _ _)) = True
isCamera _ = False

isLight :: Container -> Bool
isLight (Container _ (Light _)) = True
isLight _ = False

isShape :: Container -> Bool
isShape (Container _ (Shape _ _)) = True
isShape _ = False

raytrace :: Scene -> Int -> Int -> Color
raytrace (Scene containers config) x y =
    let
        backgroundColor = defaultColor config
        camera = head $ filter isCamera containers
        ray = generateRay camera (sceneWidth config) (sceneHeight config) x y
        intersection = getIntersection ray containers
    in
        case intersection of
            Nothing -> backgroundColor
            (Just intersectionContainer) -> getColorFromIntersection intersectionContainer containers ray backgroundColor

generateRay :: Container -> Int -> Int -> Int -> Int -> Ray
generateRay container@(Container center (Camera fov lookAt)) width height x y =
    let
        w = fromIntegral width
        h = fromIntegral height
        centerVector = eyeVector container
        rightVector = normalize (centerVector `cross` Vector 0 1 0)
        upVector = normalize (rightVector `cross` centerVector)
        halfFov = fov / 2
        aspectRatio = h / w
        halfWidth = tan $ degreesToRadians halfFov
        halfHeight = aspectRatio * halfWidth
        cameraWidth = halfWidth * 2
        cameraHeight = halfHeight * 2
        pixelWidth = cameraWidth / (w - 1)
        pixelHeight = cameraHeight / (h - 1)
        scaledX = ((fromIntegral x * pixelWidth) - halfWidth) `scalarMult` rightVector
        scaledY = ((fromIntegral y * pixelHeight) - halfHeight) `scalarMult` upVector
        orientation = normalize $ centerVector `add` scaledX `add` scaledY
    in
        Ray center orientation

eyeVector :: Container-> Vector
eyeVector (Container center (Camera fov lookAt)) = normalize $ lookAt `sub` center

-- Intersections

getIntersection :: Ray -> [Container] -> Maybe (Double, Container)
getIntersection ray containers
    | null intersections = Nothing
    | otherwise = Just $ minimumBy minimumDefinedByFirst intersections
    where intersections = mapMaybe (closestIntersection ray) containers

minimumDefinedByFirst :: (Double, Container) -> (Double, Container) -> Ordering
minimumDefinedByFirst  x y
    | fst x < fst y = LT
    | fst x > fst y = GT
    | otherwise = EQ

closestIntersection:: Ray -> Container -> Maybe (Double, Container)
closestIntersection _ (Container _ (Light _)) = Nothing
closestIntersection _ (Container _ (Camera _ _)) = Nothing
closestIntersection ray@(Ray origin direction) container@(Container center (Shape (Sphere radius) _)) =
    let
        l = origin `sub` center
        a = direction `dot` direction
        b = 2 * (direction `dot` l)
        c =  (l `dot` l) - radius^2
        listOfRoots = roots a b c
        min = minimum listOfRoots
        point = pointAlongRay ray min
    in
        case listOfRoots of
            [] -> Nothing
            otherwise -> Just (min, container)
closestIntersection ray@(Ray origin direction) container@(Container center (Shape (Plane normal) _)) =
    let 
        distance = ((center `sub` origin) `dot` normal) / (direction `dot` normal)
        point = pointAlongRay ray distance
    in 
        if distance < 0 then Nothing else Just (distance, container)

pointAlongRay :: Ray -> Double -> Vector
pointAlongRay ray distance = origin ray `add` (distance `scalarMult` direction ray)

-- Lambertian Lighting

getColorFromIntersection :: (Double, Container) -> [Container] -> Ray -> Color -> Color
getColorFromIntersection (hitDistance, hitContainer@(Container center (Shape geometry material))) containers ray backgroundColor = 
    let
        hitPoint = pointAlongRay ray hitDistance
        otherContainers = filter (/= hitContainer) containers
        visibleLights = getVisibleLights hitPoint otherContainers
    in
        lambertColor hitPoint material hitContainer visibleLights

getVisibleLights :: Vector -> [Container] -> [Container]
getVisibleLights point containers = getVisibleLights' point (filter isLight containers) (filter isShape containers)

getVisibleLights' :: Vector -> [Container] -> [Container] -> [Container] 
getVisibleLights' _ lights [] = lights
getVisibleLights' point lights shapes = filter (isLightVisible point shapes) lights

isLightVisible :: Vector -> [Container] -> Container -> Bool
isLightVisible _ [] _ = True
isLightVisible point (shape:xs) lightContainer@(Container lightCenter _) =
    let
        direction = normalize $ lightCenter `sub` point
        intersection = closestIntersection (Ray point direction) shape
    in
        maybe (isLightVisible point xs lightContainer) (const False) intersection

lambertColor :: Vector -> Material -> Container -> [Container] -> Color
lambertColor point (Material color) shape lights =
    let 
        normal = normalAtPoint point shape
        lValue = totalLambertValue point normal lights
    in 
        lValue `scalarMult` color
        
normalAtPoint :: Vector -> Container -> Vector
normalAtPoint point (Container center (Shape (Sphere _) _)) = normalize (point `sub` center)
normalAtPoint point (Container _ (Shape (Plane normal ) _)) = normal
        
totalLambertValue :: Vector -> Vector -> [Container] -> Double
totalLambertValue point normal lights = sum $ map (lambertValue point normal) lights

lambertValue :: Vector -> Vector -> Container -> Double
lambertValue point normal (Container center (Light (PointLight intensity))) =
    let lightDirection = normalize $ center `sub` point
    in intensity * max 0 (normal `dot` lightDirection)

-- Transformations
rotateAroundPoint :: Vector -> Vector -> Double -> Container -> Container
rotateAroundPoint point axis angle (Container center object) = Container (rotateAroundAxis point axis angle center) object

rotateAroundAxis :: Vector -> Vector -> Double -> Vector -> Vector
rotateAroundAxis point axis angle vector = point `add` rotate axis angle (vector `sub` point)
