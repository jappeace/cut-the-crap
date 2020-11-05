{-# LANGUAGE DataKinds #-}

module Cut.Analyze
  ( detectSoundInterval
  , Interval(..)
  , Sound
  , Silent
  , getStart
  , getEnd
  , getDuration
  , takeOnlyLines
  , detectSpeech
  )
where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Cut.Shell
import           Cut.Options
import           Cut.SpeechRecognition
import           Data.Coerce
import           Data.Foldable
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Data.Text.Lens
import           Shelly                  hiding (find, shelly)
import           Text.Regex.TDFA         hiding (empty)
import           GHC.Generics                 (Generic)

data Silent
data Sound

-- | I think we can only detect silence with ffmpeg, so we flip the
--  silent to sounded intervals, and :
-- if x indicates silence:
-- silence: xxx___xxx__xxxx____xxxxx__xx
-- sounded: ___xxx___xx____xxxx_____xx__
data Interval e =
  Interval
  { interval_start       :: Double
  , interval_end         :: Double
  , interval_duration    :: Double
  , interval_input_start :: Text
  , interval_input_end   :: Text
  } deriving (Show, Generic)

detectSoundInterval :: (MonadMask m, MonadUnliftIO m) => ListenCutOptions -> m [Interval Sound]
detectSoundInterval opts = do
  lines'' <- shelly $ detectShell opts
  let linesRes = do
        line <- lines''
        if takeOnlyLines line then pure $ Right line else pure $ Left line
      lines' = linesRes ^.. traversed . _Right

  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ putStrLn "-----------------actual lines-----------------"
  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ Text.putStrLn $ Text.unlines lines'

  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ putStrLn "-----------------filtered lines-----------------"
  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ Text.putStrLn $ Text.unlines (linesRes ^.. traversed . _Left)

  let linedUp        :: [(Text, Text)]
      linedUp        = zipped lines'
      parsed         :: [Interval Silent]
      parsed         = parse <$> linedUp
      fancyResult    :: [Interval Sound]
      fancyResult    = detector opts parsed
      negativeResult :: Maybe (Interval Sound)
      negativeResult = find ((0 >) . interval_duration) fancyResult

  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ putStrLn "-----------------lined up-----------------"
  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ traverse_ print linedUp

  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ putStrLn "-----------------parsed-----------------"
  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ traverse_ print parsed

  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ putStrLn "-----------------sounds-----------------"
  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ traverse_ print fancyResult

  if isJust negativeResult
    then do
      liftIO $ traverse_ print fancyResult
      liftIO $ print negativeResult
      error "Found negative durations"
    else pure fancyResult

  where
      detector       :: ListenCutOptions -> [Interval Silent] -> [Interval Sound]
      detector       = if opts ^. cut_noise then detectSilence else detectSound

takeOnlyLines :: Text -> Bool
takeOnlyLines matchWith = matches
 where
  silenceRegex :: String
  silenceRegex = ".*silencedetect.*"
  matches :: Bool
  matches = Text.unpack matchWith =~ silenceRegex

zipped :: [Text] -> [(Text, Text)]
zipped []                 = mempty
zipped [_               ] = []
zipped (one : two : rem') = (one, two) : zipped rem'

detectSilence :: ListenCutOptions -> [Interval Silent] -> [Interval Sound]
detectSilence _ = coerce

-- TODO: we can't process videos that are longer then a week.
finalSound :: Interval Silent
finalSound = Interval week (week+1) 1 "" ""
  where
    week = day * 7
    day = 60*60*24

detectSound :: ListenCutOptions -> [Interval Silent] -> [Interval Sound]
detectSound opts silences =
  reverse $ snd $ fun soundedParts finalSound
  where
    soundedParts :: (Interval Silent, [Interval Sound])
    soundedParts = foldl' fun (Interval 0 0 0 "" "", []) silences

    fun = flip $ silentIntoSounded opts


silentIntoSounded
  :: ListenCutOptions
  -> Interval Silent
  -> (Interval Silent, [Interval Sound])
  -> (Interval Silent, [Interval Sound])
silentIntoSounded opts current prev = (current, soundedInterval : snd prev)
 where
  soundedInterval = Interval
    { interval_start       = soundStart
    , interval_end         = soundEnd - margin
    , interval_duration    = (soundEnd - soundStart) + margin
    , interval_input_start =
      interval_input_start (fst prev) <> "," <> interval_input_end (fst prev)
    , interval_input_end   = interval_input_start current
                             <> ","
                             <> interval_input_end current
    }
  soundEnd   = interval_start current
  soundStart = interval_end $ fst prev

  margin     = opts ^. detect_margin


detectShell :: ListenCutOptions -> Sh [Text]
detectShell opt' = ffmpeg (opt' ^. lc_fileio . in_file . packed)
  ["-map"
  , voice_track_map opt'
  , "-filter:a"
                 -- , "silencedetect=noise=-30dB:d=0.5"
  , "silencedetect=noise="
  <> (opt' ^. silent_treshold . to floatToText)
  <> ":d="
  <> (opt' ^. silent_duration . to floatToText)
  , "-f"
  , "null"
  , "-"
  ]

parse :: (Text, Text) -> Interval Silent
parse xx = Interval { interval_start       = getStart $ fst xx
                    , interval_end         = getEnd $ snd xx
                    , interval_duration    = getDuration $ snd xx
                    , interval_input_start = fst xx
                    , interval_input_end   = snd xx
                    }

getStart :: Text -> Double
getStart line = read $ takeWhile (/= '\'') $ matches ^. _3
 where
  str = Text.unpack line
  matches :: (String, String, String)
  matches = str =~ startMatch

startMatch :: String
startMatch = "(.*)?: "

pipe :: String
pipe = " \\| "

getDuration :: Text -> Double
getDuration line = read $ takeWhile (/= '\'') $ match2 ^. _1
 where
  str = Text.unpack line
  match1 :: (String, String, String)
  match1 = str =~ startMatch
  match2 :: (String, String, String)
  match2 = (match1 ^. _3) =~ pipe

getEnd :: Text -> Double
getEnd line = read $ match2 ^. _3
 where
  str = Text.unpack line
  match1 :: (String, String, String)
  match1 = str =~ pipe
  match2 :: (String, String, String)
  match2 = (match1 ^. _1) =~ startMatch


-- | Detect the speech on the mkv file
detectSpeech :: ListenCutOptions -> Prelude.FilePath -> Prelude.FilePath -> Sh (Either ResultCode [WordFrame])
detectSpeech options tempdir inputFile = do
  void $ ffmpeg inputFile $ (specifyTracks options) <> [
      Text.pack tmpMp3File
    ]
  void $ ffmpeg tmpMp3File [
    "-f", "s16le", "-acodec", "pcm_s16le", "-filter:a", "aresample=resampler=soxr:osr=16000", "-ac", "1", Text.pack tmpRawFile
    ]
  liftIO $ speechAnalyses tmpRawFile
  where
    tmpMp3File = tempdir <> "/" <> "speechdetect.mp3"
    tmpRawFile = tempdir <> "/" <> "speechdetect.raw"
