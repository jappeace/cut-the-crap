module Cut.Analyze
  ( detect
  , Interval(..)
  , Sound
  , Silent
  , getStart
  , getEnd
  , getDuration
  , takeOnlyLines
  )
where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Cut.Ffmpeg
import           Cut.Options
import           Data.Foldable
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Text.Lens
import           Shelly                  hiding ( find )
import           Text.Regex.TDFA         hiding ( empty )

data Silent
data Sound

data Interval e = Interval
  { interval_start       :: Double
  , interval_end         :: Double
  , interval_duration    :: Double
  , interval_input_start :: Text
  , interval_input_end   :: Text
  } deriving Show

detect :: (MonadMask m, MonadUnliftIO m) => Options -> m [Interval Sound]
detect opts = do
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

  let linedUp        = zipped lines'
      parsed         = parse <$> linedUp
      fancyResult    = detectSound opts parsed
      negativeResult = find ((0 >) . interval_duration) fancyResult

  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ putStrLn "-----------------lined up-----------------"
  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ traverse_ print linedUp

  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ putStrLn "-----------------parsed-----------------"
  liftIO $ putStrLn "-----------------------------------------"
  liftIO $ traverse_ print parsed

  if isJust negativeResult
    then do
      liftIO $ traverse_ print fancyResult
      liftIO $ print negativeResult
      error "Found negative durations"
    else pure fancyResult

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

detectSound :: Options -> [Interval Silent] -> [Interval Sound]
detectSound opts =
  --  -- TODO figure out why these durations get recorded as < 0
  reverse . snd . foldl' (flip (compare' opts)) (Interval 0 0 0 "" "", [])

compare'
  :: Options
  -> Interval Silent
  -> (Interval Silent, [Interval Sound])
  -> (Interval Silent, [Interval Sound])
compare' opts current prev = (current, soundedInterval : snd prev)
 where
  soundedInterval = Interval
    { interval_start       = interval_end $ fst prev
    , interval_end         = interval_start current - margin
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


detectShell :: Options -> Sh [Text]
detectShell opt' = ffmpeg
  [ "-i"
  , opt' ^. in_file . packed
  , "-map"
  , "0:" <> opt' ^. voice_track . to show . packed
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
