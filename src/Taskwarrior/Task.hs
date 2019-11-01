module Taskwarrior.Task
  ( Task(..)
  )
where

import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
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
