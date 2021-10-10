{- | This Module exports the main datatype of this library: Task.
 It is provided with FromJSON and ToJSON instances.
-}
module Taskwarrior.Task (
  Task (..),
  Tag,
  makeTask,
  {-
   | == Adherence to specification
   This library uses the [taskwarrior specification for the JSON serialisation format](https://taskwarrior.org/docs/design/task.html).
   But it deviates in a small number of ways to be more pragmatic.

   * 'Task' has the fields 'id' and 'urgency' although they are technically UDAs.
   * There are two invalid states which are not prevented via the Haskell type system by the chosen modeling:

     1. A 'Task' with a 'Just' value for 'recurringChild' should not have the 'Status' 'Taskwarrior.Status.Recurring'.
     2. The 'due' field needs to be a 'Just' value on a 'Task' with 'Status' 'Taskwarrior.Status.Recurring'.
  -}
) where

import Prelude hiding (id)

import Control.Monad (join)
import Data.Aeson (
  FromJSON,
  ToJSON,
  Value,
  parseJSON,
  withObject,
  withText,
  (.:),
  (.:?),
  (.=),
 )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Foreign.Marshal.Utils (fromBool)
import Taskwarrior.Annotation (Annotation)
import Taskwarrior.Priority (Priority)
import qualified Taskwarrior.Priority as Priority
import Taskwarrior.RecurringChild (RecurringChild)
import qualified Taskwarrior.RecurringChild as RecurringChild
import Taskwarrior.Status (Status)
import qualified Taskwarrior.Status as Status
import qualified Taskwarrior.Time as Time
import Taskwarrior.UDA (UDA)

{- | A 'Task' represents a task from taskwarrior.
 The specification demands, that the existence of some fields is dependent on the status of the task.
 Those fields are therefore bundled in 'Status' as a sum-type.

 All fields in an imported task which are not part of the specification will be put in the 'UDA' (user defined attributes) 'Data.HashMap.Strict.HashMap'.

 Since the json can have multiple semantically equivalent representations of a task first serializing and then deserializing is not identity.
 But deserializing and then serializing should be. (Thus making serializing and deserializing idempotent.)
-}
data Task = Task
  { status :: Status
  , recurringChild :: Maybe RecurringChild
  , uuid :: UUID
  , id :: Maybe Integer
  , entry :: UTCTime
  , description :: Text
  , start :: Maybe UTCTime
  , modified :: Maybe UTCTime
  , due :: Maybe UTCTime
  , until :: Maybe UTCTime
  , annotations :: Set Annotation
  , scheduled :: Maybe UTCTime
  , project :: Maybe Text
  , priority :: Maybe Priority
  , depends :: Set UUID
  , tags :: Set Tag
  , urgency :: Double
  , uda :: UDA
  }
  deriving (Eq, Show, Read)

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
    status <- Status.parseFromObject object
    recurringChild <- RecurringChild.parseFromObjectMay object
    uuid <- object .: "uuid"
    idRaw <- object .:? "id"
    let id = if idRaw == Just 0 then Nothing else idRaw
    entry <- object .: "entry" >>= Time.parse
    description <- object .: "description"
    start <- parseTimeFromFieldMay "start"
    modified <- parseTimeFromFieldMay "modified"
    due <- parseTimeFromFieldMay "due"
    until_ <- parseTimeFromFieldMay "until"
    scheduled <- parseTimeFromFieldMay "scheduled"
    annotations <- Foldable.fold <$> object .:? "annotations"
    project <- object .:? "project"
    priority <-
      join
        <$> parseFromFieldWithMay Priority.parseMay object "priority"
    depends <-
      maybe
        (pure mempty)
        parseUuidList
        (HashMap.lookup "depends" object)
    tags <- Foldable.fold <$> object .:? "tags"
    urgency <- fromMaybe 0 <$> object .:? "urgency"
    pure Task{until = until_, ..}

parseFromFieldWithMay ::
  (Value -> Aeson.Types.Parser a) ->
  Aeson.Object ->
  Text ->
  Aeson.Types.Parser (Maybe a)
parseFromFieldWithMay parser object name =
  traverse parser (HashMap.lookup name object)

parseUuidList :: Aeson.Value -> Aeson.Types.Parser (Set UUID)
parseUuidList =
  withText "Text" $
    fmap Set.fromList
      . mapM (parseJSON . Aeson.String)
      . Text.splitOn ","

instance ToJSON Task where
  toJSON Task{until = until_, ..} =
    Aeson.object $
      Status.toPairs status
        <> [ "uuid" .= uuid
           , "entry" .= Time.toValue entry
           , "description" .= description
           ]
        <> ["urgency" .= urgency | urgency /= 0]
        <> maybe [] RecurringChild.toPairs recurringChild
        <> ifNotNullSet annotations ("annotations" .=)
        <> Maybe.mapMaybe
          (\(name, value) -> (name .=) . Time.toValue <$> value)
          [ ("start", start)
          , ("modified", modified)
          , ("due", due)
          , ("scheduled", scheduled)
          , ("until", until_)
          ]
        <> Maybe.catMaybes
          [ ("id" .=) <$> id
          , ("project" .=) <$> project
          , ("priority" .=) <$> priority
          ]
        <> ifNotNullSet
          depends
          ( ("depends" .=)
              . Text.intercalate ","
              . fmap UUID.toText
              . Set.toList
          )
        <> ifNotNullSet tags ("tags" .=)
        <> HashMap.toList uda

ifNotNullSet :: (Ord b) => Set b -> (Set b -> a) -> [a]
ifNotNullSet set f =
  ( Semigroup.stimesMonoid . (fromBool :: Bool -> Integer) . not . Set.null $ set
  )
    [f set]

-- | Makes a Task with the given mandatory fields uuid, entry time and description. See createTask for a non-pure version which needs less parameters.
makeTask :: UUID -> UTCTime -> Text -> Task
makeTask uuid entry description =
  Task
    { uuid
    , description
    , entry
    , id = Nothing
    , modified = Just entry
    , status = Status.Pending
    , recurringChild = Nothing
    , due = Nothing
    , priority = Nothing
    , project = Nothing
    , start = Nothing
    , scheduled = Nothing
    , until = Nothing
    , annotations = mempty
    , depends = mempty
    , tags = mempty
    , urgency = 0
    , uda = HashMap.empty
    }
