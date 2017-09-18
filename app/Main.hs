-- Copyright 2016 Boris Kachscovsky
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
module Main where

import           Camera            (Camera (..))
import           Codec.Picture
import           Codec.Picture.Png
import           Color             (pixelRGB8)
import qualified Color
import           DataTypes         (Config (..), Light (..), Material (..),
                                    Object (..), Scene (..))
import           Render            (getCoordColor)
import           Shapes            (Shape (..))
import           Transformations   (rotateCamera)
import           Vector            (Vector (..))

main :: IO ()
main =
  let objects =
        [ Object (Sphere (Vector (-0.5) 0 2) 0.5) (Material Color.red)
        , Object (Sphere (Vector 0 0 3) 0.5) (Material Color.blue)
        , Object
            (Plane (Vector 0 (-0.5) 0) (Vector 0 1 0))
            (Material Color.pink)
        , Object (Plane (Vector 0 0 5) (Vector 0 0 (-1))) (Material Color.pink)
        ]
      lights =
        [ PointLight (Vector 0 0.5 0) 0.4
        , PointLight (Vector 0.5 0.5 0) 0.4
        , PointLight (Vector 9 0 4) 0.2
        ]
      camera =
        rotateCamera
          (Vector 0 0 3)
          (Vector 0 1 0)
          (-90)
          (Camera 45 (Vector 0 0 (-1)) (Vector 0 0 3))
      config = Config 500 500 Color.black
      scene = Scene objects lights camera config
      width = sceneWidth config
      height = sceneHeight config
      img =
        generateImage
          (\x y -> pixelRGB8 $ getCoordColor scene x (height - y))
          width
          height
  in writePng "output.png" img
