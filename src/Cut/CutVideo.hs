{-# OPTIONS_GHC -Wno-type-defaults #-}

module Cut.CutVideo
  ( extract
  , Interval(..)
  , Silent
  , Sound
  , combine
  , combineOutput
  )
where

import qualified Control.Foldl       as Fl
import           Control.Lens
import           Control.Monad.Catch
import           Cut.Analyze
import           Cut.Ffmpeg
import           Cut.Options
import           Data.Foldable
import qualified Data.Text           as Text
import           Data.Text.Lens
import           Turtle              hiding (FilePath, has, options)
import qualified Turtle              as T

specifyTracks :: Options -> [Text]
specifyTracks options = if has (music_track . _Just) options
  then -- note that using copy breaks timing eg: ["-c:v", "copy", "-c:a", "copy"]
        [ "-map"
         , "0:0"
         , "-map"  -- then copy only the voice track
         , "0:" <> options ^. voice_track . to show . packed
         ]
  else []

toArgs :: Options -> FilePath -> Interval Sound -> (Interval Sound, [Text])
toArgs options tmp inter =
  ( inter
  , -- keep ter interval for debugging
    ["-y", "-ss", start, "-t", duration, "-i", options ^. in_file . packed]
    <> specifyTracks options
    <> [ Text.pack tmp
         <> "/"
         <> options
         ^. out_file
         .  packed
         <> "-"
         <> fname
         <> ".mkv"
       ]
  )
 where
  start    = floatToText $ interval_start inter
  duration = floatToText $ interval_duration inter
  fname    = Text.pack $ show $ truncate $ interval_start inter * 100

extract :: Options -> FilePath -> [Interval Sound] -> IO ()
extract options tempDir intervals = do
  traverse_
      (\(inter, args) -> void $ catch (T.fold (ffmpeg args) Fl.list) $ \exec ->
        do
          liftIO
            (print
              ("expection during edit: ", exec :: SomeException, args, inter)
            )
          pure [Left "expection"]
      )
    $   toArgs options tempDir
    <$> intervals
  liftIO $ putStrLn "finish extracting"

combineOutput :: FilePath
combineOutput = "combined-output.mkv"

combine :: FilePath -> Shell ()
combine tempfiles = do
  output' <- ffmpeg args
  liftIO $ print ("output", output')
 where -- https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
  args =
    [ "-y"
    , "-f"
    , "concat"
    , "-safe"
    , "0"
    , "-i"
    , Text.pack (tempfiles <> "/input.txt")
    , "-c"
    , "copy"
    , Text.pack $ tempfiles <> "/" <> combineOutput
    ]
