module Taskwarrior.UDA
  ( UDA
  )
where

import           Data.Text                      ( Text )
import           Data.HashMap.Strict            ( HashMap )
import           Data.Aeson                     ( Value )

type UDA = HashMap Text Value
