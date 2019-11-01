{-# LANGUAGE RecordWildCards #-}
module TaskSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Aeson
import           Taskwarrior.Task
import           Taskwarrior.Mask
import           Taskwarrior.Status
import           Taskwarrior.Annotation
import           Taskwarrior.Priority
import           Data.Time
import qualified Data.HashMap.Strict           as HashMap
import           Test.QuickCheck.Instances.Text ( )
import           Test.QuickCheck.Instances.UUID ( )
import           Test.QuickCheck.Instances.UnorderedContainers
                                                ( )


prop_taskDeEncode :: Task -> Property
prop_taskDeEncode task = Just task === decode (encode task)

prop_taskReadShow :: Task -> Property
prop_taskReadShow task = task === (read . show $ task)

spec :: Spec
spec = do
  it "will be the same after read . show" $ property prop_taskReadShow
  it "will be the same after encoding to JSON and decoding"
    $ property prop_taskDeEncode

instance Arbitrary MaskState where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Mask where
  arbitrary = Mask <$> (arbitrary :: Gen [MaskState])

instance Arbitrary Status where
  arbitrary = oneof
    [ pure Pending
    , Deleted <$> arbitrary
    , Completed <$> arbitrary
    , Waiting <$> arbitrary
    , RecurringParent <$> arbitrary <*> arbitrary
    , RecurringChild <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary UTCTime where
  arbitrary = do
    day     <- ModifiedJulianDay <$> arbitrary
    dayTime <- secondsToDiffTime <$> choose (0, 86400)
    pure $ UTCTime day dayTime

instance Arbitrary Annotation where
  arbitrary = do
    entry       <- arbitrary
    description <- arbitrary
    pure Annotation { .. }

instance Arbitrary Priority where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Task where
  arbitrary = do
    status      <- arbitrary
    uuid        <- arbitrary
    entry       <- arbitrary
    description <- arbitrary
    start       <- arbitrary
    modified    <- arbitrary
    due         <- arbitrary
    until_      <- arbitrary
    annotations <- arbitrary
    scheduled   <- arbitrary
    project     <- arbitrary
    priority    <- arbitrary
    depends     <- arbitrary
    tags        <- arbitrary
    uda <- HashMap.fromList . fmap (\(x, y) -> (x, String y)) <$> arbitrary
    pure Task { until = until_, .. }
