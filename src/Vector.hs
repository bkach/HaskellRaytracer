module Vector where

data Vector = Vector {x :: Double, y :: Double, z :: Double} deriving(Eq, Show)
type Point = Vector

-- Helper Functions
--
add :: Vector -> Vector -> Vector
(Vector x y z) `add` (Vector x1 y1 z1) = Vector (x + x1) (y + y1) (z + z1)

sub :: Vector -> Vector -> Vector
(Vector x y z) `sub` (Vector x1 y1 z1) = Vector (x - x1) (y - y1) (z - z1)

scalarMult :: Double -> Vector -> Vector
s `scalarMult` (Vector x y z) = Vector (x * s) (y * s) (z * s)

dot :: Vector -> Vector -> Double
(Vector x y z) `dot` (Vector x1 y1 z1) = x * x1 + y * y1 + z * z1

cross :: Vector -> Vector -> Vector
(Vector x y z) `cross` (Vector x1 y1 z1) = Vector (y * z1 - z * y1) (z * x1 - x * z1) (x * y1 - y * x1)

normalize :: Vector -> Vector
normalize (Vector x y z) = let m = magnitude (Vector x y z)
                           in Vector (x / m) (y / m) (z / m)

magnitude :: Vector -> Double
magnitude (Vector x y z) = sqrt (x * x + y * y + z * z)

neg :: Vector -> Vector
neg = scalarMult (-1)

data Quaternion = Quaternion Double Vector

vectorToQuaternion :: Vector -> Quaternion
vectorToQuaternion = Quaternion 0

quaternionToVector :: Quaternion -> Vector
quaternionToVector (Quaternion _ vector) = vector

rotationQuaternion :: Vector -> Double -> Quaternion
rotationQuaternion (Vector x y z) angle = 
    let
        halfAngle = 0.5 * angle
        c = cos halfAngle
        s = sin halfAngle
    in
        Quaternion c (Vector (x * s) (y * s) (z * s))

conjugate :: Quaternion -> Quaternion
conjugate (Quaternion w vector) = Quaternion w (neg vector)

quatMult :: Quaternion -> Quaternion -> Quaternion
(Quaternion w1 v1) `quatMult` (Quaternion w2 v2) = Quaternion (w1 * w2 - (v1 `dot` v2)) ((v1 `cross` v2) `add` (w1 `scalarMult` v2) `add` (w2 `scalarMult` v1))

degreesToRadians :: Double -> Double
degreesToRadians deg = (pi / 180) * deg

rotate :: Vector -> Double -> Vector -> Vector
rotate axis angleInDegrees point = 
    let 
        angle = degreesToRadians angleInDegrees
        normalizedAxis = normalize axis
        rotQuat = rotationQuaternion normalizedAxis angle
        rotQuatConjugate = conjugate rotQuat
        pointQuat = vectorToQuaternion point
    in
        quaternionToVector $ (rotQuat `quatMult` pointQuat) `quatMult` rotQuatConjugate
