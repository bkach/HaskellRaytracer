module RayCaster.DataTypes where

import RayCaster.Vector
import RayCaster.Camera
import RayCaster.Color
import RayCaster.Shapes
import RayCaster.Ray

-- Basic Data Types
data Object = Object Shape Material deriving(Eq)
data Material = Material Color deriving(Eq)
data Light = PointLight {center :: Vector, intensity :: Double} -- center, intensity
data Scene = Scene [Object] [Light] Camera Config
data Config = Config { sceneWidth :: Int,
                       sceneHeight :: Int,
                       defaultColor :: Color }
