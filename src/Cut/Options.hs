{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Cut.Options
  ( Options
  , in_file
  , out_file
  , seg_size
  , silent_treshold
  , detect_margin
  , voice_track
  , music_path
  , silent_duration
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
  , detectMargin = halp . _Just # def_margin
  , voiceTrack = halp . _Just # 2
  , musicPath = halp # Nothing
  , silentDuration = halp . _Just # def_duration
  }

data Options = Options
  { inFile :: FilePath <?> "The input video"
  , outFile :: FilePath <?> "The output name without format"
  , segmentSize :: (Maybe Int) <?> "The size of video segments in minutes"
  , silentTreshold :: (Maybe Double) <?> "The treshold for determining intersting sections, closer to zero is detects more audio (n: https://ffmpeg.org/ffmpeg-filters.html#silencedetect)"
  , silentDuration :: (Maybe Double) <?> "The duration before soemthing can be considered a silence (d: https://ffmpeg.org/ffmpeg-filters.html#silencedetect)"
  , detectMargin :: (Maybe Double) <?> "Margin seconds around detection"
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

def_margin :: Double
def_margin = 0.05

def_silent :: Double
def_silent = 0.0001

def_duration :: Double
def_duration = 0.25

def_voice :: Int
def_voice = 2

seg_size :: Lens' Options Int
seg_size = field @"segmentSize" . halp . non def_seg_size

detect_margin :: Lens' Options Double
detect_margin = field @"detectMargin" . halp . non def_margin

silent_treshold :: Lens' Options Double
silent_treshold = field @"silentTreshold" . halp . non def_silent

silent_duration :: Lens' Options Double
silent_duration = field @"silentDuration" . halp . non def_duration

voice_track :: Lens' Options Int
voice_track = field @"voiceTrack" . halp . non def_voice

music_path :: Lens' Options (Maybe FilePath)
music_path = field @"musicPath" . halp

instance ParseRecord Options
