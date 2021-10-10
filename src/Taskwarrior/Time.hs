-- | This module provides no own time type for taskwarrior rather it only gives deserialisation and serialisation support.
module Taskwarrior.Time (
  parse,
  toValue,
) where

import Data.Aeson (withText)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (
  Parser,
  typeMismatch,
 )
import qualified Data.Text as Text
import Data.Time (
  UTCTime,
  defaultTimeLocale,
  parseTimeM,
 )
import qualified Data.Time.Format as Time.Format

-- | Converts a time to the taskwarrior time format.
toValue :: UTCTime -> Aeson.Value
toValue time =
  Aeson.String . Text.pack $
    Time.Format.formatTime
      defaultTimeLocale
      "%Y%m%dT%H%M%SZ"
      time

-- | Parses a JSON string from the taskwarrior time format.
parse :: Aeson.Value -> Parser UTCTime
parse value =
  withText
    "Date"
    ( maybe (typeMismatch "Date" value) pure
        . parseTimeM False defaultTimeLocale "%Y%m%dT%H%M%SZ"
        . Text.unpack
    )
    value
