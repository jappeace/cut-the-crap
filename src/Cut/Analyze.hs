module Cut.Analyze
  ( detect, Interval(..), Sound, Silent, getStart , getEnd , getDuration,
  ) where

import qualified Control.Foldl           as Fl
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Cut.Ffmpeg
import           Cut.Options
import           Data.Either
import           Data.Foldable
import qualified Data.Text               as Text
import           Data.Text.Lens
import           Text.Regex.TDFA         hiding (empty)
import           Turtle                  hiding (FilePath)
import qualified Turtle                  as Sh

data Silent
data Sound

data Interval e = Interval
  { interval_start    :: Double
  , interval_end      :: Double
  , interval_duration :: Double
  } deriving Show

detect :: (MonadMask m, MonadUnliftIO m) => Options -> m [Interval Sound]
detect opts = do
  lines' <- Sh.fold (detectShell opts) $ Fl.prefilter (either (("[silencedetect" ==) . Text.take (Text.length "[silencedetect") . Sh.lineToText) (const False)) Fl.list
  let
      linedUp :: [(Sh.Line, Sh.Line)]
      linedUp = do
        elems <- imap (\i a -> (i, a)) $ zip (take (length lines' - 1) lines') (drop 1 lines')
        if even (fst elems) then
          pure (bimap (fromLeft mempty) (fromLeft mempty) $ snd elems)
        else empty
      parsed = parse <$> linedUp
  pure $ detectSound opts parsed


detectSound :: Options -> [Interval Silent] -> [Interval Sound]
detectSound opts = filter ((0 <) . interval_duration) -- TODO figure out why these durations get recorded as < 0
  . reverse . snd . foldl'
  (flip (compare' opts))
  ((Interval 0 0 0, []))

compare'
  :: Options -> Interval Silent
  -> (Interval Silent, [Interval Sound])
  -> (Interval Silent, [Interval Sound])
compare' opts x' y = (x', soundedInterval : snd y)
 where
  soundedInterval = Interval { interval_start    = interval_end $ fst y
                             , interval_end      = interval_start x' - margin
                             , interval_duration = (soundEnd - soundStart) + margin
                             }
  soundEnd   = interval_start x'
  soundStart = interval_end $ fst y

  margin = opts ^. detect_margin


detectShell :: Options -> Shell (Either Line Line)
detectShell opt' =
  ffmpeg ["-i"
                 , opt' ^. in_file . packed
                 , "-map"
                 , "0:" <> opt' ^. voice_track . to show . packed
                 , "-filter:a"
                 -- , "silencedetect=noise=-30dB:d=0.5"
                 , "silencedetect=noise=" <>
                    (opt' ^. silent_treshold . to floatToText)
                   <> ":d=" <>
                    (opt' ^. silent_duration . to floatToText)
                 , "-f"
                 , "null"
                 , "-"
                 ]



parse :: (Sh.Line, Sh.Line) -> Interval Silent
parse xx = Interval {
   interval_start    = getStart $ fst xx
  , interval_end      = getEnd $ snd xx
  , interval_duration = getDuration $ snd xx
  }

getStart :: Sh.Line -> Double
getStart line = read $ matches ^. _3
  where
  str = Text.unpack $ Sh.lineToText line
  matches :: (String, String, String)
  matches = str =~ startMatch

startMatch :: String
startMatch = "(.*)?: "

pipe :: String
pipe = " \\| "

getDuration :: Sh.Line -> Double
getDuration line = read $ match2 ^. _1
  where
  str = Text.unpack $ Sh.lineToText line
  match1 :: (String, String, String)
  match1 = str =~ startMatch
  match2 :: (String, String, String)
  match2 = (match1 ^. _3) =~ pipe

getEnd :: Sh.Line -> Double
getEnd line = read $ match2 ^. _3
  where
  str = Text.unpack $ Sh.lineToText line
  match1 :: (String, String, String)
  match1 = str =~ pipe
  match2 :: (String, String, String)
  match2 = (match1 ^. _1) =~ startMatch
