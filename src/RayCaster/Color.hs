module RayCaster.Color where

data Color = Color Double Double Double deriving (Eq, Show)

red :: Color
red = Color 1 0 0

green :: Color
green = Color 0 1 0

blue :: Color
blue = Color 0 0 1

white :: Color
white = Color 1 1 1

black :: Color
black = Color 0 0 0

pink :: Color
pink = Color 1 0.5 0.5

colorMult :: Double -> Color -> Color
colorMult v (Color r g b) = Color (r * v) (g * v) (b * v)

colorSurfaceInteraction :: Color -> Color -> Color
colorSurfaceInteraction (Color surfR surfG surfB) (Color lightR lightG lightB) =
  (Color (surfR * lightR / 255) (surfG * lightG / 255) (surfB * lightB / 255))

colorAdd :: Color -> Color -> Color
colorAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)


