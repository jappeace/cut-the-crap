{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Cut.Options
  ( ProgramOptions
  , FileIO
  , ListenCutOptions
  , parseProgram
  , lc_fileio
  , in_file
  , out_file
  , seg_size
  , silent_treshold
  , detect_margin
  , voice_track
  , music_track
  , silent_duration
  , cut_noise
  , work_dir
  , simpleOptions
  , voice_track_map
  , specifyTracks
  , getOutFileName
  , gnerate_sub_prism
  , listen_cut_prism
  )
where

import           Control.Lens
import           Data.Generics.Product.Fields
import           Data.Generics.Sum
import qualified Data.Text                    as Text
import           Data.Text.Lens
import           GHC.Generics                 hiding (to)
import           Options.Applicative

simpleFileIO :: FileIO
simpleFileIO = FileIO  { fi_inFile         = "in.mkv"                      , fi_outFile        = "out.mkv"
                       , fi_workDir        = Nothing
                        }

in_file :: Lens' FileIO FilePath
in_file = field @"fi_inFile"

out_file :: Lens' FileIO FilePath
out_file = field @"fi_outFile"

work_dir :: Lens' FileIO (Maybe FilePath)
work_dir = field @"fi_workDir"

simpleOptions :: ListenCutOptions
simpleOptions = ListenCutOptions
                        { lc_fileIO = simpleFileIO
                        , lc_segmentSize    = _Just # def_seg_size
                        , lc_silentTreshold = _Just # def_silent
                        , lc_detectMargin   = _Just # def_margin
                        , lc_voiceTrack     = _Just # 2
                        , lc_musicTrack     = Nothing
                        , lc_silentDuration = _Just # def_duration
                        , lc_cutNoise       = def_cut_noise
                        }

getOutFileName :: ListenCutOptions -> FilePath
getOutFileName = reverse . takeWhile ('/' /=) . reverse . view (lc_fileio . out_file)

-- | Deals with having an input file and a target output file
data FileIO = FileIO
              { fi_inFile  :: FilePath
              , fi_outFile :: FilePath
              , fi_workDir :: Maybe FilePath -- ^ for consistency (or debugging) we may want to specify this.
              }
  deriving (Show, Generic)

-- | Cut out by listening to sound options
data ListenCutOptions = ListenCutOptions
                { lc_fileIO         :: FileIO
                , lc_segmentSize    :: Maybe Int
                , lc_silentTreshold :: Maybe Double
                , lc_silentDuration :: Maybe Double
                , lc_detectMargin   :: Maybe Double
                , lc_voiceTrack     :: Maybe Int
                , lc_musicTrack     :: Maybe Int
                , lc_cutNoise       :: Bool
                }
  deriving (Show, Generic)

data ProgramOptions = ListenCut ListenCutOptions
                    | GenerateSubtitles FileIO
  deriving (Show, Generic)

listen_cut_prism :: Prism' ProgramOptions ListenCutOptions
listen_cut_prism = _Ctor @"ListenCut"

gnerate_sub_prism :: Prism' ProgramOptions FileIO
gnerate_sub_prism = _Ctor @"GenerateSubtitles"

def_seg_size :: Int
def_seg_size = 20

def_margin :: Double
def_margin = 0.05

def_cut_noise :: Bool
def_cut_noise = False

def_silent :: Double
def_silent = 0.0001

def_duration :: Double
def_duration = 0.25

def_voice :: Int
def_voice = 1

lc_fileio :: Lens' ListenCutOptions FileIO
lc_fileio = field @"lc_fileIO"

seg_size :: Lens' ListenCutOptions Int
seg_size = field @"lc_segmentSize" . non def_seg_size

detect_margin :: Lens' ListenCutOptions Double
detect_margin = field @"lc_detectMargin" . non def_margin

silent_treshold :: Lens' ListenCutOptions Double
silent_treshold = field @"lc_silentTreshold" . non def_silent

silent_duration :: Lens' ListenCutOptions Double
silent_duration = field @"lc_silentDuration" . non def_duration

voice_track :: Lens' ListenCutOptions Int
voice_track = field @"lc_voiceTrack" . non def_voice

music_track :: Lens' ListenCutOptions (Maybe Int)
music_track = field @"lc_musicTrack"

cut_noise :: Lens' ListenCutOptions Bool
cut_noise = field @"lc_cutNoise"

voice_track_map :: ListenCutOptions -> Text.Text
voice_track_map = mappend "0:" . view (voice_track . to show . packed)

specifyTracks :: ListenCutOptions -> [Text.Text]
specifyTracks options =
  [ "-map"
  , "0:0"
  , "-map"  -- then copy only the voice track
  , voice_track_map options
  ]

parseFile :: Parser FileIO
parseFile = FileIO
    <$> option str (long "inFile" <> help "The input video")
    <*> option str (long "outFile" <> help "The output name without format")
    <*> optional
          (option
            str
            (  long "workDir"
            <> help
                 "If specified will use this as temporary directory to store intermeidate files in, good for debugging. Needs to be absolute"
            )
          )

parseProgram :: Parser ProgramOptions
parseProgram =
  subparser $
    command "listen" (info (ListenCut <$> parseSound) $ progDesc "Cut out by listening to sound options. We listen for silences and cut out the parts that are silenced.")
    <>
    command "subtitles" (info (GenerateSubtitles <$> parseFile) $ progDesc "Generate subtiles for a video. This is an intermediate (but usefull) feature developed for recognizing human speech vs background noise.")

parseSound :: Parser ListenCutOptions
parseSound = ListenCutOptions
    <$> parseFile
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
    <*> switch
          (long "cutNoise" <> help "Whether to cut noise instead of silence")
