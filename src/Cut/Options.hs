{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Cut.Options
  ( Options
  , parseRecord
  , in_file
  , out_file
  , seg_size
  , silent_treshold
  , detect_margin
  , voice_track
  , music_track
  , silent_duration
  , work_dir
  , simpleOptions
  )
where

import           Control.Lens                   ( (#)
                                                , Lens'
                                                , _Just
                                                , non
                                                )
import           Data.Generics.Product.Fields
import           GHC.Generics
import           Options.Applicative

simpleOptions :: Options
simpleOptions = Options { inFile         = "in.mkv"
                        , outFile        = "out.mkv"
                        , segmentSize    = _Just # def_seg_size
                        , silentTreshold = _Just # def_silent
                        , detectMargin   = _Just # def_margin
                        , voiceTrack     = _Just # 2
                        , musicTrack     = Nothing
                        , silentDuration = _Just # def_duration
                        , workDir        = Nothing
                        }

data Options = Options
  { inFile :: FilePath
  , outFile :: FilePath
  , segmentSize :: Maybe Int
  , silentTreshold :: Maybe Double
  , silentDuration :: Maybe Double
  , detectMargin :: Maybe Double
  , voiceTrack :: Maybe Int
  , musicTrack :: Maybe Int
  , workDir :: Maybe FilePath
  } deriving (Show, Generic)

in_file :: Lens' Options FilePath
in_file = field @"inFile"

out_file :: Lens' Options FilePath
out_file = field @"outFile"

def_seg_size :: Int
def_seg_size = 20

def_margin :: Double
def_margin = 0.05

def_silent :: Double
def_silent = 0.0001

def_duration :: Double
def_duration = 0.25

def_voice :: Int
def_voice = 1

seg_size :: Lens' Options Int
seg_size = field @"segmentSize" . non def_seg_size

detect_margin :: Lens' Options Double
detect_margin = field @"detectMargin" . non def_margin

silent_treshold :: Lens' Options Double
silent_treshold = field @"silentTreshold" . non def_silent

silent_duration :: Lens' Options Double
silent_duration = field @"silentDuration" . non def_duration

voice_track :: Lens' Options Int
voice_track = field @"voiceTrack" . non def_voice

music_track :: Lens' Options (Maybe Int)
music_track = field @"musicTrack"

work_dir :: Lens' Options (Maybe FilePath)
work_dir = field @"workDir"

parseRecord :: Parser Options
parseRecord =
  Options
    <$> option str (long "inFile" <> help "The input video")
    <*> option str (long "outFile" <> help "The output name without format")
    <*> optional
          (option
            auto
            (long "segmentSize" <> help "The size of video segments in minutes")
          )
    <*> optional
          (option
            auto
            (  long "silentTreshold"
            <> help
                 "The treshold for determining intersting sections, closer to zero is detects more audio (n: https://ffmpeg.org/ffmpeg-filters.html#silencedetect)"
            )
          )
    <*> optional
          (option
            auto
            (  long "silentDuration"
            <> help
                 "The duration before soemthing can be considered a silence (d: https://ffmpeg.org/ffmpeg-filters.html#silencedetect)"
            )
          )
    <*> optional
          (option
            auto
            (long "detectMargin" <> help "Margin seconds around detection")
          )
    <*> optional
          (option
            auto
            (long "voiceTrack" <> help "The track to detect the silences upon")
          )
    <*> optional
          (option auto (long "musicTrack" <> help "The track to integrate"))
    <*> optional
          (option
            str
            (  long "workDir"
            <> help
                 "If specified will use this as temporary directory to store intermeidate files in, good for debugging. Needs to be absolute"
            )
          )
