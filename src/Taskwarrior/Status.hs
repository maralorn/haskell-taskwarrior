-- | This module deals with information of a task which is dependent on the status.
module Taskwarrior.Status
  ( Status(..)
  , parseFromObject
  , toPairs
  )
where

import           Taskwarrior.Mask               ( Mask )
import qualified Taskwarrior.Time              as Time
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
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.Aeson.Types               ( Parser
                                                , typeMismatch
                                                , Pair
                                                )

-- | A task can be pending, deleted, completed, waiting or recurring.
-- It is recommended to access the fields only by pattern matching since the getters are partial.
data Status =
  Pending |
  Deleted {  end :: UTCTime } |
  Completed {  end :: UTCTime } |
  Waiting { wait :: UTCTime } |
  Recurring {
    recur :: Text,
    mask :: Mask}
  deriving (Eq, Show, Read, Ord)

-- | Takes all information that is dependent on the status from a JSON object.
parseFromObject :: Object -> Parser Status
parseFromObject o = (o .: "status") >>= \case
  "pending"   -> pure Pending
  "deleted"   -> Deleted <$> (o .: "end" >>= Time.parse)
  "completed" -> Completed <$> (o .: "end" >>= Time.parse)
  "waiting"   -> Waiting <$> (o .: "wait" >>= Time.parse)
  "recurring" -> Recurring <$> o .: "recur" <*> o .: "mask"
  str         -> typeMismatch "status" (Aeson.String str)

-- | A list of Pairs can be used to construct a JSON object later. The result of 'toPairs' is supposed to be combined with the rest of the fields of a task.
toPairs :: Status -> [Pair]
toPairs = \case
  Pending        -> [statusLabel "pending"]
  Deleted {..}   -> [statusLabel "deleted", "end" .= Time.toValue end]
  Completed {..} -> [statusLabel "completed", "end" .= Time.toValue end]
  Waiting {..}   -> [statusLabel "waiting", "wait" .= Time.toValue wait]
  Recurring {..} -> [statusLabel "recurring", "recur" .= recur, "mask" .= mask]
 where
  statusLabel :: Text -> Pair
  statusLabel = ("status" .=)

instance FromJSON Status where
  parseJSON = withObject "Status" parseFromObject

instance ToJSON Status where
  toJSON     = object . toPairs
  toEncoding = pairs . mconcat . map (uncurry (.=)) . toPairs
