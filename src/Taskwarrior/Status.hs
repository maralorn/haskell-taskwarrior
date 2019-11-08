{-# LANGUAGE LambdaCase #-}
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
                                                )
import qualified Data.Aeson                    as Aeson
import           Control.Applicative            ( (<|>) )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.UUID                      ( UUID )
import           Data.Aeson.Types               ( Parser
                                                , typeMismatch
                                                , Pair
                                                )
-- | A task can be pending, deleted, completed, waiting or recurring.
-- If I task is a recurring child or a recurring parent depends on the existence of the corresponding fields and can not be told from the status field alone.
-- It is recommended to access the fields only by pattern matching since the getters are partial.
data Status =
  Pending |
  Deleted {  end :: UTCTime } |
  Completed {  end :: UTCTime } |
  Waiting { wait :: UTCTime } |
  RecurringParent {
    recur :: Text,
    mask :: Mask} |
  RecurringChild {
    recur :: Text,
    imask :: Integer,
    parent :: UUID }
  deriving (Eq, Show, Read, Ord)

parseFromObject, parseParentFromObject, parseChildFromObject
  :: Object -> Parser Status
-- | Takes all information that is dependent on the status from a JSON object.
parseFromObject o = (o .: "status") >>= \case
  "pending"   -> pure Pending
  "deleted"   -> Deleted <$> (o .: "end" >>= Time.parse)
  "completed" -> Completed <$> (o .: "end" >>= Time.parse)
  "waiting"   -> Waiting <$> (o .: "wait" >>= Time.parse)
  "recurring" -> parseParentFromObject o <|> parseChildFromObject o
  str         -> typeMismatch "status" (Aeson.String str)

-- | Gathers all fields for a RecurringChild status.
parseChildFromObject o =
  RecurringChild <$> o .: "recur" <*> o .: "imask" <*> o .: "parent"

-- | Gathers all fields for a RecurringParent status.
parseParentFromObject o = RecurringParent <$> o .: "recur" <*> o .: "mask"

-- | A list of Pairs can be used to construct a JSON object later. The result of Status.toPairs is supposed to be combined with the rest of the fields of a task.
toPairs :: Status -> [Pair]
toPairs = \case
  Pending        -> [statusLabel "pending"]
  Deleted {..}   -> [statusLabel "deleted", "end" .= Time.toValue end]
  Completed {..} -> [statusLabel "completed", "end" .= Time.toValue end]
  Waiting {..}   -> [statusLabel "waiting", "wait" .= Time.toValue wait]
  RecurringParent {..} ->
    [statusLabel "recurring", "recur" .= recur, "mask" .= mask]
  RecurringChild {..} ->
    [ statusLabel "recurring"
    , "recur" .= recur
    , "imask" .= imask
    , "parent" .= parent
    ]
 where
  statusLabel :: Text -> Pair
  statusLabel = ("status" .=)
