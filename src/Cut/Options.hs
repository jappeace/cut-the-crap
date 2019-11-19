{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Cut.Options
  ( Options
  , in_file
  , out_file
  , seg_size
  , silent_treshold
  , frame_margin
  , voice_track
  , music_path
  , simpleOptions
  ) where

import           Control.Lens
import           Data.Generics.Product.Fields
import           Options.Generic

simpleOptions :: Options
simpleOptions = Options
  { inFile = halp # "in.mkv"
  , outFile = halp # "out.mkv"
  , segmentSize = halp . _Just # def_seg_size
  , silentTreshold = halp . _Just # def_silent
  , frameMargin = halp . _Just # def_margin
  , voiceTrack = halp . _Just # 2
  , musicPath = halp # Nothing
  }


data Options = Options
  { inFile :: FilePath <?> "The input video"
  , outFile :: FilePath <?> "The output name without format"
  , segmentSize :: (Maybe Int) <?> "The size of video segments in minutes"
  , silentTreshold :: (Maybe Float) <?> "The treshold for determining intersting sections"
  , frameMargin :: (Maybe Int) <?> "Margin in frames around interesting sections"
  , voiceTrack :: (Maybe Int) <?> "The track to detect audio upon"
  , musicPath :: (Maybe FilePath) <?> "The music track"
  } deriving (Show, Generic)

halp :: Iso' (a <?> b) a
halp = iso unHelpful Helpful

in_file :: Lens' Options FilePath
in_file = field @"inFile" . halp

out_file :: Lens' Options FilePath
out_file = field @"outFile" . halp

def_seg_size :: Int
def_seg_size = 20

def_margin :: Int
def_margin = 1

def_silent :: Float
def_silent = 0.3

def_voice :: Int
def_voice = 2

seg_size :: Lens' Options Int
seg_size = field @"segmentSize" . halp . non def_seg_size

frame_margin :: Lens' Options Int
frame_margin = field @"frameMargin" . halp . non def_margin

silent_treshold :: Lens' Options Float
silent_treshold = field @"silentTreshold" . halp . non def_silent

voice_track :: Lens' Options Int
voice_track = field @"voiceTrack" . halp . non def_voice

music_path :: Lens' Options (Maybe FilePath)
music_path = field @"musicPath" . halp

instance ParseRecord Options
