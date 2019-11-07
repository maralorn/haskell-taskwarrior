{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
-- |
module Taskwarrior.Annotation
  ( Annotation(..)
  )
where

import qualified Taskwarrior.Time              as Time
import           Data.Time                      ( UTCTime )
import           Data.Text                      ( Text )
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                )
import qualified Data.Aeson                    as Aeson

-- |
data Annotation = Annotation { entry :: UTCTime, description :: Text } deriving (Eq, Show, Read, Ord)

instance Aeson.FromJSON Annotation where
  parseJSON = Aeson.withObject "Annotation" $ \o -> do
    description <- o .: "description"
    entry       <- o .: "entry" >>= Time.parse
    pure Annotation { .. }

instance Aeson.ToJSON Annotation where
  toJSON Annotation {..} =
    Aeson.object ["description" .= description, "entry" .= Time.toValue entry]
