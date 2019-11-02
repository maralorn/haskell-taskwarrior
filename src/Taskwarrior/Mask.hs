{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Taskwarrior.Mask
  ( Mask(..)
  , MaskState
  )
where

import qualified Data.Text                     as Text
import           Data.String.Interpolate        ( i )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson.Types

data MaskState = Pending | Completed | Deleted | Waiting deriving (Eq, Show, Enum, Read, Ord, Bounded)

newtype Mask = Mask {mask :: [MaskState]} deriving (Eq, Read, Ord, Show)

toChar :: MaskState -> Char
toChar = \case
  Pending   -> '.'
  Completed -> '+'
  Deleted   -> 'X'
  Waiting   -> 'W'

instance Aeson.FromJSON Mask where
  parseJSON =
    Aeson.withText "Mask" $ fmap Mask . traverse parseChar . Text.unpack

parseChar :: Char -> Aeson.Types.Parser MaskState
parseChar = \case
  '.'  -> pure Pending
  '+'  -> pure Completed
  'X'  -> pure Deleted
  'W'  -> pure Waiting
  char -> fail [i|Not a Mask Char: '#{char}'|]

instance Aeson.ToJSON Mask where
  toJSON = Aeson.String . Text.pack . fmap toChar . mask
