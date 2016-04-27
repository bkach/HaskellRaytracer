module Ray where

import Vector

data Ray = Ray {origin :: Vector, direction :: Vector}

pointAlongRay :: Ray -> Double -> Vector
pointAlongRay ray distance = origin ray `add` (distance `scalarMult` direction ray)
