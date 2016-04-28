module Intersection where

import DataTypes

type Intersection = (Double, Object)

closest :: Intersection -> Intersection -> Ordering
closest x y
  | fst x < fst y = LT
  | fst x > fst y = GT
  | otherwise = EQ


