{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Cut.Options
  ( Options
  , in_file
  , out_file
  , seg_size
  , silent_treshold
  , frame_margin
  , voice_track
  , music_path
  ) where

import Control.Lens
import Data.Generics.Product.Fields
import Options.Generic

data Options = Options
  { inFile :: FilePath <?> "The input video"
  , outFile :: FilePath <?> "The output name without format"
  , segmentSize :: (Maybe Int) <?> "The size of video segments in minutes"
  , silentTreshold :: (Maybe Float) <?> "The treshold for determining intersting sections"
  , frameMargin :: (Maybe Int) <?> "Margin in frames around interesting sections"
  , voiceTrack :: Int <?> "The track to detect audio upon"
  , musicPath :: FilePath <?> "The music track"
  } deriving (Show, Generic)

halp :: Iso' (a <?> b) a
halp = iso unHelpful Helpful

in_file :: Lens' Options FilePath
in_file = field @"inFile" . halp

out_file :: Lens' Options FilePath
out_file = field @"outFile" . halp

seg_size :: Lens' Options Int
seg_size = field @"segmentSize" . halp . non 20

frame_margin :: Lens' Options Int
frame_margin = field @"frameMargin" . halp . non 1

silent_treshold :: Lens' Options Float
silent_treshold = field @"silentTreshold" . halp . non 0.03

voice_track :: Lens' Options Int
voice_track = field @"voiceTrack" . halp

music_path :: Lens' Options FilePath
music_path = field @"musicPath" . halp

instance ParseRecord Options
