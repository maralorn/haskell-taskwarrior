-- | This Module exports the main datatype of this library: Task.
-- It is provided with FromJSON and ToJSON instances.
module Taskwarrior.Task
  ( Task(..)
  )
where

import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import qualified Data.UUID                     as UUID
import           Data.UUID                      ( UUID )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson.Types
import           Data.Aeson                     ( withObject
                                                , withText
                                                , FromJSON
                                                , ToJSON
                                                , parseJSON
                                                , (.:)
                                                , (.=)
                                                , (.:?)
                                                , Value
                                                )
import qualified Data.Semigroup                as Semigroup
import qualified Data.Maybe                    as Maybe
import           Control.Monad                  ( join )
import qualified Data.Foldable                 as Foldable
import           Taskwarrior.Status             ( Status )
import qualified Taskwarrior.Status            as Status
import           Taskwarrior.Priority           ( Priority )
import qualified Taskwarrior.Priority          as Priority
import           Taskwarrior.UDA                ( UDA )
import           Taskwarrior.Annotation         ( Annotation )
import qualified Taskwarrior.Time              as Time
import qualified Data.HashMap.Strict           as HashMap
import           Foreign.Marshal.Utils          ( fromBool )


type Tag = Text

-- | A Task represents task from taskwarrior. See <https://taskwarrior.org/docs/design/task.html> for the specification of the fields.
-- The specification demands, that the existence of some fields is dependent on the status of the task.
-- Those fields are therefore bundled in status as a sum-type.
--
-- All fields in an imported task which are not part of the specification will be put in the UDA (user defined attributes) HashMap.
--
-- Since the json can have multiple semantically equivalent representation of a task first serializing and then deserializing is not identity.
-- But deserializing and then serializing should be. (Thus making serializing and deserializing idempotent.)
data Task = Task {
        status      :: Status,
        uuid        :: UUID,
        entry       :: UTCTime,
        description :: Text,
        start       :: Maybe UTCTime,
        modified    :: Maybe UTCTime,
        due         :: Maybe UTCTime,
        until       :: Maybe UTCTime,
        annotations :: [Annotation],
        scheduled   :: Maybe UTCTime,
        project     :: Maybe Text,
        priority    :: Maybe Priority,
        depends     :: [UUID],
        tags        :: [Tag],
        uda         :: UDA
} deriving (Eq, Show, Read)

reservedKeys :: [Text]
reservedKeys =
  [ "status"
  , "uuid"
  , "description"
  , "entry"
  , "modified"
  , "due"
  , "until"
  , "scheduled"
  , "annotations"
  , "start"
  , "project"
  , "priority"
  , "depends"
  , "tags"
  , "wait"
  , "end"
  , "mask"
  , "imask"
  , "parent"
  , "recur"
  ]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \object -> do
    let parseTimeFromFieldMay = parseFromFieldWithMay Time.parse object
        uda = HashMap.filterWithKey (\k _ -> k `notElem` reservedKeys) object
    status      <- Status.parseFromObject object
    uuid        <- object .: "uuid"
    entry       <- object .: "entry" >>= Time.parse
    description <- object .: "description"
    start       <- parseTimeFromFieldMay "start"
    modified    <- parseTimeFromFieldMay "modified"
    due         <- parseTimeFromFieldMay "due"
    until_      <- parseTimeFromFieldMay "until"
    scheduled   <- parseTimeFromFieldMay "scheduled"
    annotations <- Foldable.fold <$> object .:? "annotations"
    project     <- object .:? "project"
    priority    <- join
      <$> parseFromFieldWithMay Priority.parseMay object "priority"
    depends <- maybe (pure []) parseUuidList (HashMap.lookup "depends" object)
    tags    <- Foldable.fold <$> object .:? "tags"
    pure Task { until = until_, .. }

parseFromFieldWithMay
  :: (Value -> Aeson.Types.Parser a)
  -> Aeson.Object
  -> Text
  -> Aeson.Types.Parser (Maybe a)
parseFromFieldWithMay parser object name =
  traverse parser (HashMap.lookup name object)

parseUuidList :: Aeson.Value -> Aeson.Types.Parser [UUID]
parseUuidList =
  withText "Text" $ mapM (parseJSON . Aeson.String) . Text.splitOn ","

instance ToJSON Task where
  toJSON Task { until = until_, ..} =
    Aeson.object
      $  Status.toPairs status
      <> [ "uuid" .= uuid
         , "entry" .= Time.toValue entry
         , "description" .= description
         ]
      <> ifNotNullList annotations ("annotations" .=)
      <> Maybe.mapMaybe
           (\(name, value) -> (name .=) . Time.toValue <$> value)
           [ ("start"    , start)
           , ("modified" , modified)
           , ("due"      , due)
           , ("scheduled", scheduled)
           , ("until"    , until_)
           ]
      <> Maybe.catMaybes
           [("project" .=) <$> project, ("priority" .=) <$> priority]
      <> ifNotNullList
           depends
           (("depends" .=) . Text.intercalate "," . fmap UUID.toText)
      <> ifNotNullList tags ("tags" .=)
      <> HashMap.toList uda

ifNotNullList :: [b] -> ([b] -> a) -> [a]
ifNotNullList list f =
  (Semigroup.stimesMonoid . (fromBool :: Bool -> Integer) . not . null $ list)
    [f list]
