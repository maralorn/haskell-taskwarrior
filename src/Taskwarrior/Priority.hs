-- | This module provides the type for the 'Priority' of a task.
module Taskwarrior.Priority
  ( parseMay
  , Priority(..)
  )
where

import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types               ( Parser )


-- | A 'Taskwarrior.Task.Task' can have the priorities 'High', 'Medium', 'Low' or none, which is modeled via a 'Maybe' 'Priority'.
data Priority = High | Medium | Low
        deriving (Eq, Show, Read, Enum, Ord, Bounded)

instance Aeson.ToJSON Priority where
  toJSON = \case
    High   -> "H"
    Medium -> "M"
    Low    -> "L"

-- | Parses a JSON string to a 'Maybe' 'Priority', fails on anything else.
parseMay :: Aeson.Value -> Parser (Maybe Priority)
parseMay = Aeson.withText "Priority" $ \case
  "H" -> pure $ Just High
  "M" -> pure $ Just Medium
  "L" -> pure $ Just Low
  ""  -> pure Nothing
  s ->
    fail
      $  "parsing Priority failed, unexpected "
      ++ show s
      ++ " (expected \"H\", \"M\", \"L\", or \"\")"

instance Aeson.FromJSON Priority where
  parseJSON val = parseMay val >>= \case
    Nothing ->
      fail
        "parsing Priority failed, unexpected null (expected \"H\", \"M\", or \"L\")"
    Just p -> pure p
