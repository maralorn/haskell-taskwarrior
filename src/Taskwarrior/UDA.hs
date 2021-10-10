-- | User defined attributes are stored in a 'Map' from 'Text' to json 'Value's because we have no type information about them.
module Taskwarrior.UDA (
  UDA,
) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)

-- | A field will in practice only be a number or a string.
type UDA = Map Text Value
