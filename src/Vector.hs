module Vector where

data Vector = Vector Double Double Double deriving(Show)

-- Helper Functions
--
add :: Vector -> Vector -> Vector
(Vector x y z) `add` (Vector x1 y1 z1) = Vector (x + x1) (y + y1) (z + z1)

sub :: Vector -> Vector -> Vector
(Vector x y z) `sub` (Vector x1 y1 z1) = Vector (x - x1) (y - y1) (z - z1)

scalarMult :: Vector -> Double -> Vector
(Vector x y z) `scalarMult` s = Vector (x * s) (y * s) (z * s)

dotProduct :: Vector -> Vector -> Double
(Vector x y z) `dotProduct` (Vector x1 y1 z1) = x * x1 + y * y1 + z * z1

cross :: Vector -> Vector -> Vector
(Vector x y z) `cross` (Vector x1 y1 z1) = Vector (y * z1 + z *y1) (x * z1 + y * x1) (x * y1 + y * x1)

normalize :: Vector -> Vector
normalize (Vector x y z) = let m = magnitude (Vector x y z)
                           in Vector (x / m) (y / m) (z / m)

magnitude :: Vector -> Double
magnitude (Vector x y z) = sqrt (x * x + y * y + z * z)

neg :: Vector -> Vector
neg (Vector x y z) = Vector (-x) (-y) (-z)

