module Transformations where

import DataTypes
import Vector
import Quaternion
import Camera

-- Transformations
rotateCamera :: Vector -> Vector -> Double -> Camera -> Camera
rotateCamera point axis angle (Camera fov center lookingAt) = 
    let
        newCenter = point `add` rotate axis angle (center `sub` point)
    in
        Camera fov newCenter lookingAt

rotateObj :: Vector -> Vector -> Double -> Object -> Object
rotateObj point axis angle (Object shape material) = 
    let
        rotatedShape = translateShape point (rotateShape axis angle (translateShape (neg point) shape))
    in
        Object rotatedShape material

rotateShape :: Vector -> Double -> Shape -> Shape
rotateShape axis angle (Sphere center radius) = Sphere (rotate axis angle center) radius
rotateShape axis angle (Plane center normal) = Plane (rotate axis angle center) normal

translateObj :: Vector -> Object -> Object
translateObj translationVector (Object shape material) = Object (translateShape translationVector shape) material

translateShape :: Vector -> Shape -> Shape
translateShape translationVector (Sphere center radius) = Sphere (center `add` translationVector) radius
translateShape translationVector (Plane center normal) = Plane (center `add` translationVector) normal

translateCamera :: Vector -> Camera -> Camera
translateCamera translationVector (Camera fov center lookAt) = Camera fov (center `add` translationVector) lookAt

scaleObj :: Object -> Double -> Object
scaleObj (Object shape material) factor = Object (scaleShape factor shape) material

scaleShape :: Double -> Shape -> Shape
scaleShape factor (Sphere center radius) = Sphere center (factor * radius)
scaleShape factor shape = shape
