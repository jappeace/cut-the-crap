{-# OPTIONS_GHC -Wno-type-defaults #-}

module Cut.CutVideo
  ( edit
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

toArgs :: Options -> FilePath -> Interval Sound -> [Text]
toArgs opt' tmp inter =
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
  ]
 where
  start    = Text.pack $ show $ interval_start inter
  duration = Text.pack $ show $ interval_duration inter

  fname = Text.pack $ show $ truncate $ interval_start inter * 100

edit :: Options -> FilePath -> [Interval Sound] -> IO ()
edit opt' tempDir intervals = do
  liftIO $ print intervals
  traverse_
      (\args -> do
        liftIO $ print args
        out <- catch (T.fold (ffmpeg args) Fl.list) $ \exec -> do
          liftIO (print ("expection during edit: ", exec :: SomeException, args))
          pure [Left "expection"]
        liftIO $ print ("finisehd with ", out)
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
