module Colour where

import Data.Word (Word8)
import Codec.Picture (PixelRGB8(..))

type Colour = (Word8, Word8, Word8)

colour2Px :: Colour -> PixelRGB8
colour2Px (r, g, b) = PixelRGB8 r g b

red :: Colour
red = (255, 0, 0)

green :: Colour
green = (0, 255, 0)

blue :: Colour
blue = (0, 0, 255)

white :: Colour
white = (255, 255, 255)

black :: Colour
black = (0, 0, 0)

