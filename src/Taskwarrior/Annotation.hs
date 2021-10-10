-- | Provides the 'Annotation' type with 'Data.Aeson.ToJSON' and 'Data.Aeson.FromJSON' instances.
module Taskwarrior.Annotation (
  Annotation (..),
) where

import Data.Aeson (
  (.:),
  (.=),
 )
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Taskwarrior.Time as Time

-- | A taskwarrior 'Taskwarrior.Task.Task' can have multiple annotations. They contain a timestamp 'entry' and a 'description'.
data Annotation = Annotation {entry :: UTCTime, description :: Text} deriving (Eq, Show, Read, Ord)

instance Aeson.FromJSON Annotation where
  parseJSON = Aeson.withObject "Annotation" $ \o -> do
    description <- o .: "description"
    entry <- o .: "entry" >>= Time.parse
    pure Annotation{..}

instance Aeson.ToJSON Annotation where
  toJSON Annotation{..} =
    Aeson.object ["description" .= description, "entry" .= Time.toValue entry]
