-- | This Module exports the main datatype of this library: Task.
-- It is provided with FromJSON and ToJSON instances.
--
module Taskwarrior.Task
  ( Task(..)
  , Tag
  , makeTask
  -- | == Adherence to specification
  -- This library uses the [taskwarrior specification for the JSON serialisation format](https://taskwarrior.org/docs/design/task.html).
  -- But it deviates in a small number of ways to be more pragmatic.
  --
  -- * 'Task' has the fields 'id' and 'urgency' although they are technically UDAs.
  -- * There are two invalid states which are not prevented via the Haskell type system by the chosen modeling:
  --
  --   1. A 'Task' with a 'Just' value for 'recurringChild' should not have the 'Status' 'Taskwarrior.Status.Recurring'.
  --   2. The 'due' field needs to be a 'Just' value on a 'Task' with 'Status' 'Taskwarrior.Status.Recurring'.
  )
where

import           Prelude                 hiding ( id )

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
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Maybe                    as Maybe
import           Control.Monad                  ( join )
import qualified Data.Foldable                 as Foldable
import           Taskwarrior.Status             ( Status )
import qualified Taskwarrior.Status            as Status
import           Taskwarrior.RecurringChild     ( RecurringChild )
import qualified Taskwarrior.RecurringChild    as RecurringChild
import           Taskwarrior.Priority           ( Priority )
import qualified Taskwarrior.Priority          as Priority
import           Taskwarrior.UDA                ( UDA )
import           Taskwarrior.Annotation         ( Annotation )
import qualified Taskwarrior.Time              as Time
import qualified Data.HashMap.Strict           as HashMap
import           Foreign.Marshal.Utils          ( fromBool )

-- | A 'Task' represents a task from taskwarrior.
-- The specification demands, that the existence of some fields is dependent on the status of the task.
-- Those fields are therefore bundled in 'Status' as a sum-type.
--
-- All fields in an imported task which are not part of the specification will be put in the 'UDA' (user defined attributes) 'Data.HashMap.Strict.HashMap'.
--
-- Since the json can have multiple semantically equivalent representations of a task first serializing and then deserializing is not identity.
-- But deserializing and then serializing should be. (Thus making serializing and deserializing idempotent.)
data Task = Task {
        status         :: Status,
        recurringChild :: Maybe RecurringChild,
        uuid           :: UUID,
        id             :: Maybe Integer,
        entry          :: UTCTime,
        description    :: Text,
        start          :: Maybe UTCTime,
        modified       :: Maybe UTCTime,
        due            :: Maybe UTCTime,
        until          :: Maybe UTCTime,
        annotations    :: [Annotation],
        scheduled      :: Maybe UTCTime,
        project        :: Maybe Text,
        priority       :: Maybe Priority,
        depends        :: [UUID],
        tags           :: [Tag],
        urgency        :: Double,
        uda            :: UDA
} deriving (Eq, Show, Read)

-- | A Tag can be basically any string. But beware: Special symbols work but might clash with @task@ cli syntax. As an example you can use a space in a @'Tag'@. But then you cannot use @task +my tag@ on the command line.
type Tag = Text


reservedKeys :: [Text]
reservedKeys =
  [ "status"
  , "uuid"
  , "id"
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
  , "urgency"
  ]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \object -> do
    let parseTimeFromFieldMay = parseFromFieldWithMay Time.parse object
        uda = HashMap.filterWithKey (\k _ -> k `notElem` reservedKeys) object
    status         <- Status.parseFromObject object
    recurringChild <- RecurringChild.parseFromObjectMay object
    uuid           <- object .: "uuid"
    idRaw          <- object .:? "id"
    let id = if idRaw == Just 0 then Nothing else idRaw
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
    urgency <- maybe 0 (\x -> x) <$> object .:? "urgency"
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
         , "id" .= fromMaybe 0 id
         , "entry" .= Time.toValue entry
         , "description" .= description
         , "urgency" .= urgency
         ]
      <> maybe [] RecurringChild.toPairs recurringChild
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

-- | Makes a Task with the given mandatory fields uuid, entry time and description. See createTask for a non-pure version which needs less parameters.
makeTask :: UUID -> UTCTime -> Text -> Task
makeTask uuid entry description = Task { uuid
                                       , description
                                       , entry
                                       , id             = Nothing
                                       , modified       = Just entry
                                       , status         = Status.Pending
                                       , recurringChild = Nothing
                                       , due            = Nothing
                                       , priority       = Nothing
                                       , project        = Nothing
                                       , start          = Nothing
                                       , scheduled      = Nothing
                                       , until          = Nothing
                                       , annotations    = []
                                       , depends        = []
                                       , tags           = []
                                       , urgency        = 0
                                       , uda            = HashMap.empty
                                       }
