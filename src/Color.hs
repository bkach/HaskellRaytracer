module Color where

import Data.Word (Word8)
import Codec.Picture (PixelRGB8(..))

type Color = (Word8, Word8, Word8)

pixelRGB8 :: Color -> PixelRGB8
pixelRGB8  (r, g, b) = PixelRGB8 r g b

red :: Color
red = (255, 0, 0)

green :: Color
green = (0, 255, 0)

blue :: Color
blue = (0, 0, 255)

white :: Color
white = (255, 255, 255)

black :: Color
black = (0, 0, 0)

