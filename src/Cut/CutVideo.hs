{-# OPTIONS_GHC -Wno-type-defaults #-}

module Cut.CutVideo
  ( extract
  , Interval(..)
  , Silent
  , Sound
  , combine
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
import           Turtle              hiding (FilePath)
import qualified Turtle              as T

toArgs :: Options -> FilePath -> Interval Sound -> (Interval Sound, [Text])
toArgs opt' tmp inter = (inter, -- keep ter interval for debugging
  [ "-y"
  , "-ss"
  , start
  , "-t"
  , duration
  , "-i"
  , opt' ^. in_file . packed
  , Text.pack tmp
    <> "/"
    <> opt' ^. out_file .  packed
    <> "-"
    <> fname
    <> ".mkv"
  , "-c"
  , "copy"
  -- , "-qscale:v"
  -- , "1"
  ])
 where
  start    = floatToText $ interval_start inter
  duration = floatToText $ interval_duration inter

  fname = Text.pack $ show $ truncate $ interval_start inter * 100

extract :: Options -> FilePath -> [Interval Sound] -> IO ()
extract opt' tempDir intervals = do
  traverse_
      (\(inter, args) -> do
        void $ catch (T.fold (ffmpeg args) Fl.list) $ \exec -> do
          liftIO (print ("expection during edit: ", exec :: SomeException, args, inter))
          pure [Left "expection"]
      )
    $   toArgs opt' tempDir
    <$> intervals
  liftIO $ putStrLn "donee"

combine :: Options -> FilePath -> Shell ()
combine opt' tempfiles = do
  output' <- ffmpeg args
  liftIO $ print ("output", output')
 where -- https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
  args = [ "-y"
         , "-f"
         , "concat"
         , "-safe"
         , "0"
        , "-i"
        , Text.pack tempfiles
        , "-c"
        , "copy"
        , opt' ^. out_file . packed
        ]
