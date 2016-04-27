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
(Vector x y z) `cross` (Vector x1 y1 z1) =
  Vector (y * z1 - z * y1) (z * x1 - x * z1) (x * y1 - y * x1)

normalize :: Vector -> Vector
normalize (Vector x y z) =
  let
    m = magnitude (Vector x y z)
  in
    Vector (x / m) (y / m) (z / m)

magnitude :: Vector -> Double
magnitude (Vector x y z) = sqrt (x * x + y * y + z * z)

neg :: Vector -> Vector
neg = scalarMult (-1)
