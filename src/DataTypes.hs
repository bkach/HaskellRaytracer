module DataTypes where

import Vector
import Camera
import Color

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
