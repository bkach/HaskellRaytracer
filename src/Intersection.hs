module Intersection where

import Data.Ord

import DataTypes

data Intersection = Intersection {
  distance :: Double,
  intersectedObject :: Object
} deriving Eq

instance Ord Intersection where
  compare x y
    | distance x < distance y = LT
    | distance x > distance y = GT
    | otherwise = EQ
  x <= y = distance x <= distance y

