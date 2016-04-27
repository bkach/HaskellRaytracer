module Shapes where

import Vector
import Ray
import Utils (roots)

data Shape = Sphere Vector Double  -- center, radius
           | Plane Vector Vector  deriving(Eq) -- center, normal

normalAtPoint :: Vector -> Shape -> Vector
normalAtPoint point (Sphere center radius) = normalize (point `sub` center)
normalAtPoint point (Plane center normal)  = normal

rayIntersection :: Ray -> Shape -> Maybe Double
rayIntersection (Ray origin direction) (Sphere center radius) =
  let
    l = origin `sub` center
    a = direction `dot` direction
    b = 2 * (direction `dot` l)
    c =  (l `dot` l) - radius^2
    listOfRoots = roots a b c
    min = minimum listOfRoots
  in
    case listOfRoots of
        []        -> Nothing
        otherwise -> if min < 0 then
                       Nothing
                     else
                       Just min
rayIntersection (Ray origin direction) (Plane center normal) =
  let
    distance = ((center `sub` origin) `dot` normal) / (direction `dot` normal)
  in
    if distance < 0 then
      Nothing
    else
      Just distance
