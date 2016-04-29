module Color where

import Codec.Picture (PixelRGB8(..))

data Color = Color Double Double Double deriving (Eq, Show)

pixelRGB8 :: Color -> PixelRGB8
pixelRGB8 (Color r g b)  = PixelRGB8 (truncate (r * 255)) (truncate (g * 255)) (truncate (b * 255))

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



