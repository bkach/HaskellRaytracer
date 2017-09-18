module RayCaster.Quaternion where

import           RayCaster.Utils
import           RayCaster.Vector

data Quaternion =
  Quaternion Double
             Vector

vectorToQuaternion :: Vector -> Quaternion
vectorToQuaternion = Quaternion 0

quaternionToVector :: Quaternion -> Vector
quaternionToVector (Quaternion _ vector) = vector

rotationQuaternion :: Vector -> Double -> Quaternion
rotationQuaternion (Vector x y z) angle =
  let halfAngle = 0.5 * angle
      c = cos halfAngle
      s = sin halfAngle
  in Quaternion c (Vector (x * s) (y * s) (z * s))

conjugate :: Quaternion -> Quaternion
conjugate (Quaternion w vector) = Quaternion w (neg vector)

quatMult :: Quaternion -> Quaternion -> Quaternion
(Quaternion w1 v1) `quatMult` (Quaternion w2 v2) =
  Quaternion
    (w1 * w2 - (v1 `dot` v2))
    ((v1 `cross` v2) `add` (w1 `scalarMult` v2) `add` (w2 `scalarMult` v1))

rotate :: Vector -> Double -> Vector -> Vector
rotate axis angleInDegrees point =
  let angle = degreesToRadians angleInDegrees
      normalizedAxis = normalize axis
      rotQuat = rotationQuaternion normalizedAxis angle
      rotQuatConjugate = conjugate rotQuat
      pointQuat = vectorToQuaternion point
  in quaternionToVector $
     (rotQuat `quatMult` pointQuat) `quatMult` rotQuatConjugate
