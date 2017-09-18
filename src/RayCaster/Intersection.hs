module RayCaster.Intersection where

import           Data.Ord

import           RayCaster.DataTypes

data Intersection = Intersection
  { intersectedObject :: Object
  , distance          :: Double
  } deriving (Eq)

instance Ord Intersection where
  compare x y
    | distance x < distance y = LT
    | distance x > distance y = GT
    | otherwise = EQ
  x <= y = distance x <= distance y
