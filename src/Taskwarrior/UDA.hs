-- | User defined attributes are stored in a HashMap from Text to json Values because we have no type information about them.
module Taskwarrior.UDA
  ( UDA
  )
where

import           Data.Text                      ( Text )
import           Data.HashMap.Strict            ( HashMap )
import           Data.Aeson                     ( Value )

-- | A field will in practice only be a number or a string.
type UDA = HashMap Text Value
