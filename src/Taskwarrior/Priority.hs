module Taskwarrior.Priority (parseMay, Priority) where

import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types               ( Parser, typeMismatch )

data Priority = High | Medium | Low
        deriving (Eq, Show, Read, Enum, Ord, Bounded)

instance Aeson.ToJSON Priority where
   toJSON = \case
      High -> "H"
      Medium -> "M"
      Low -> "L"

parseMay :: Aeson.Value -> Parser (Maybe Priority)
parseMay val = Aeson.withText "Priority" (\case
  "H" -> pure $ Just High
  "M" -> pure $ Just Medium
  "L" -> pure $ Just Low
  "" -> pure Nothing
  _ -> typeMismatch "Priority" val) val
