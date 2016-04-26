module Shapes where

import Vector
import Ray
import Utils (roots)

data Shape = Sphere Vector Double  -- center, radius
           | Plane Vector Vector  deriving(Eq) -- center, normal

