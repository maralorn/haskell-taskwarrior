{-# LANGUAGE LambdaCase #-}
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
parseFromObject o = (o .: "status") >>= \case
  "pending"   -> pure Pending
  "deleted"   -> Deleted <$> (o .: "end" >>= Time.parse)
  "completed" -> Completed <$> (o .: "end" >>= Time.parse)
  "waiting"   -> Waiting <$> (o .: "wait" >>= Time.parse)
  "recurring" -> parseParentFromObject o <|> parseChildFromObject o
  str         -> typeMismatch "status" (Aeson.String str)

parseChildFromObject o =
  RecurringChild <$> o .: "recur" <*> o .: "imask" <*> o .: "parent"

parseParentFromObject o = RecurringParent <$> o .: "recur" <*> o .: "mask"

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
