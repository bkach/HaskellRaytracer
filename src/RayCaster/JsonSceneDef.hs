{-# LANGUAGE OverloadedStrings #-}

module RayCaster.JsonSceneDef where

import           Control.Applicative  (empty)
import           Data.Aeson           (FromJSON (..), (.:))
import qualified Data.Aeson           as A
import           Data.Aeson.Types     (Parser)
import           Data.ByteString.Lazy (ByteString)

import           RayCaster.Camera     (Camera (..))
import           RayCaster.Color      (Color (..))
import           RayCaster.DataTypes  (Config (..), Light (..), Material (..),
                                       Object (..), Scene (..))
import           RayCaster.Shapes     (Shape (..))
import           RayCaster.Vector     (Vector (..))

instance FromJSON Camera where
  parseJSON (A.Object v) =
    Camera <$> v .: "fov" <*> v .: "cameraPosition" <*> v .: "lookingAt"
  parseJSON _ = empty

instance FromJSON Object where
  parseJSON (A.Object v) = Object <$> v .: "shape" <*> v .: "material"
  parseJSON _            = empty

instance FromJSON Material where
  parseJSON (A.Object v) = Material <$> v .: "color"
  parseJSON _            = empty

instance FromJSON Color where
  parseJSON (A.Object v) = Color <$> v .: "red" <*> v .: "green" <*> v .: "blue"
  parseJSON _ = empty

instance FromJSON Light where
  parseJSON (A.Object v) = PointLight <$> v .: "center" <*> v .: "intensity"
  parseJSON _            = empty

instance FromJSON Config where
  parseJSON (A.Object v) =
    Config <$> v .: "width" <*> v .: "height" <*> v .: "defaultColor"
  parseJSON _ = empty

instance FromJSON Scene where
  parseJSON (A.Object v) =
    Scene <$> v .: "objects" <*> v .: "lights" <*> v .: "camera" <*>
    v .: "config"
  parseJSON _ = empty

parseShape :: String -> A.Object -> Parser Shape
parseShape shapeType v =
  case shapeType of
    "sphere" -> Sphere <$> v .: "center" <*> v .: "radius"
    "plane"  -> Plane <$> v .: "center" <*> v .: "normal"
    _        -> empty

instance FromJSON Shape where
  parseJSON (A.Object v) = do
    t <- v .: "type"
    parseShape t v
  parseJSON _ = empty

instance FromJSON Vector where
  parseJSON (A.Object v) = Vector <$> v .: "x" <*> v .: "y" <*> v .: "z"
  parseJSON _            = empty

readJsonScene :: ByteString -> Maybe Scene
readJsonScene = A.decode
