module Transformations where

import DataTypes
import Vector
import Quaternion

-- Transformations
rotateObj :: Vector -> Double -> Object -> Object
rotateObj axis angle (Object shape material) = Object (rotateShape axis angle shape) material

rotateObjAroundPoint :: Vector -> Vector -> Double -> Object -> Object
rotateObjAroundPoint point axis angle (Object shape material) = Object (rotateShapeAroundPoint point axis angle shape) material

rotateShapeAroundPoint :: Vector -> Vector -> Double -> Shape -> Shape
rotateShapeAroundPoint point axis angle shape = translateShape point (rotateShape axis angle (translateShape (neg point) shape))

rotateShape :: Vector -> Double -> Shape -> Shape
rotateShape axis angle (Sphere center radius) = Sphere (rotate axis angle center) radius
rotateShape axis angle (Plane center normal) = Plane (rotate axis angle center) normal

translateObj :: Vector -> Object -> Object
translateObj translationVector (Object shape material) = Object (translateShape translationVector shape) material

translateShape :: Vector -> Shape -> Shape
translateShape translationVector (Sphere center radius) = Sphere (center `add` translationVector) radius
translateShape translationVector (Plane center normal) = Plane (center `add` translationVector) normal

scaleObj :: Object -> Double -> Object
scaleObj (Object shape material) factor = Object (scaleShape factor shape) material

scaleShape :: Double -> Shape -> Shape
scaleShape factor (Sphere center radius) = Sphere center (factor * radius)
scaleShape factor shape = shape
