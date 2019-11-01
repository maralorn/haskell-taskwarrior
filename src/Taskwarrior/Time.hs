module Taskwarrior.Time
  ( parse
  , toValue
  )
where
import           Data.Aeson                     ( withText )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types               ( Parser
                                                , typeMismatch
                                                )
import           Data.Time                      ( UTCTime
                                                , parseTimeM
                                                , defaultTimeLocale
                                                )
import qualified Data.Time.Format              as Time.Format
import qualified Data.Text                     as Text

toValue :: UTCTime -> Aeson.Value
toValue time = Aeson.String . Text.pack $ Time.Format.formatTime
  defaultTimeLocale
  "%Y%m%dT%H%M%SZ"
  time

parse :: Aeson.Value -> Parser UTCTime
parse value = withText
  "Date"
  ( maybe (typeMismatch "Date" value) pure
  . parseTimeM False defaultTimeLocale "%Y%m%dT%H%M%SZ"
  . Text.unpack
  )
  value
