module Color where

import Data.Word (Word8)
import Vector
import Codec.Picture (PixelRGB8(..))

type Color = Vector

pixelRGB8 :: Color -> PixelRGB8
pixelRGB8 (Vector r g b)  = PixelRGB8 (truncate r) (truncate g) (truncate b)

red :: Color
red = Vector 255 0 0

green :: Color
green = Vector 0 255 0

blue :: Color
blue = Vector 0 0 255

white :: Color
white = Vector 255 255 255

black :: Color
black = Vector 0 0 0

pink :: Color
pink = Vector 240 128 128
