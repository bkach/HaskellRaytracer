-- Copyright 2016 Boris Kachscovsky
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
main :: IO()
main = print $ Circle (normalize $ Vector 1 2 3 `cross` Vector 4 5 6) 3

-- Basic Data Types
data Vector = Vector Double Double Double deriving(Show)
data Shape = Circle { position :: Vector, radius :: Double}
           | Plane { position :: Vector, normal :: Vector }
           | Triangle Vector Vector Vector
            deriving (Show)

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

-- Finds a,b, and c for a^2*x + b*x + c*x = 0, useful for finding intersections
roots :: Double -> Double -> Double -> [Double]
roots a b c = findRootsWithDescriminant (b * b - 4 * a *c) a b

findRootsWithDescriminant :: Double -> Double -> Double -> [Double]
findRootsWithDescriminant desc a b
    | desc == 0 = [0.5 * (-b)]
    | desc > 0 = [0.5 * (-b + sqrt desc), 0.5 * (-b - sqrt desc)]
    | otherwise = []
