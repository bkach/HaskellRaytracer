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

import           Codec.Picture
import           Codec.Picture.Png
import qualified Data.ByteString.Lazy   as BS

import           RayCaster              (Config (..), Scene (..))
import           RayCaster.Color        (Color (..))
import           RayCaster.JsonSceneDef (readJsonScene)
import           RayCaster.Render       (getCoordColor)

main :: IO ()
main = do
  jsonScene <- BS.readFile "./scenes/basic.json"
  let maybeScene = readJsonScene jsonScene
  maybe (print "Scene could not be parsed") (render "output.png") maybeScene

render :: String -> Scene -> IO ()
render filename scene@(Scene _ _ _ config) =
  let width = sceneWidth config
      height = sceneHeight config
      img =
        generateImage
          (\x y -> pixelRGB8 $ getCoordColor scene x (height - y))
          width
          height
  in writePng filename img

pixelRGB8 :: Color -> PixelRGB8
pixelRGB8 (Color r g b) =
  PixelRGB8 (truncate (r * 255)) (truncate (g * 255)) (truncate (b * 255))
