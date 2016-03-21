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
module Main where

import Vector

main :: IO()
main = print $ Circle (normalize $ Vector 1 2 3 `cross` Vector 4 5 6) 3

-- Basic Data Types
data Shape = Circle Vector Double
           | Plane Vector Vector
           | Triangle Vector Vector Vector
            deriving (Show, Eq)

-- Finds a,b, and c for a^2*x + b*x + c*x = 0, useful for finding intersections
roots :: Double -> Double -> Double -> [Double]
roots a b c
    | descriminant == 0 = [0.5 * (-b)]
    | descriminant > 0 = [0.5 * (-b + sqrt descriminant), 0.5 * (-b - sqrt descriminant)]
    | otherwise = []
    where descriminant = b * b - 4 * a * c
