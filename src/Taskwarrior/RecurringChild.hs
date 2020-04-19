-- | This Module provides the RecurringChild type with
-- FromJSON and ToJSON instances.
module Taskwarrior.RecurringChild
  ( RecurringChild(..)
  , parseFromObjectMay
  , toPairs
  )
where

import           Control.Applicative            ( optional )
import           Data.Aeson                     ( Object
                                                , (.:)
                                                , (.=)
                                                , ToJSON
                                                , FromJSON
                                                , pairs
                                                , object
                                                , withObject
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types               ( Parser
                                                , Pair
                                                )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )

-- | The 'RecurringChild' type saves information about how a 'Task'
-- is child of another 'Task' wich is recurring.
data RecurringChild =
  RecurringChild {
  recur :: Text,
  imask :: Integer,
  parent :: UUID }
  deriving (Eq, Show, Read, Ord)

-- | Gathers all fields for a 'RecurringChild' status.
parseFromObjectMay :: Object -> Parser (Maybe RecurringChild)
parseFromObjectMay = optional . parseFromObject

parseFromObject :: Object -> Parser RecurringChild
parseFromObject o =
  RecurringChild <$> o .: "recur" <*> o .: "imask" <*> o .: "parent"

-- | Can be used to serialize 'RecurringChild' to JSON.
toPairs :: RecurringChild -> [Pair]
toPairs RecurringChild {..} =
  ["recur" .= recur, "imask" .= imask, "parent" .= parent]

instance FromJSON RecurringChild where
  parseJSON = withObject "RecurringChild" parseFromObject

instance ToJSON RecurringChild where
  toJSON     = object . toPairs
  toEncoding = pairs . mconcat . map (uncurry (.=)) . toPairs
