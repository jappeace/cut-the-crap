module Cut.SplitVideo
  ( split, detect
  ) where

import           Control.Lens
import           Cut.Options
import           Data.Text.Lens
import           Turtle         hiding (FilePath)

split :: FilePath -> Options -> Shell ()
split tmp opt' = do
  procs "ffmpeg" ["-i"
                 , (tmp ^. packed)
                 , "-c"
                 , "copy"
                 , "-map"
                 , "0"
                 , "-segment_time"
                 , "00:" <> (opt' ^. seg_size . to show . packed) <> ":00"
                 , "-f"
                 , "segment"
                 , "-reset_timestamps"
                 , "1"
                 , opt' ^. out_file . packed <> "%03d.mp4"
                 ] $ pure mempty

detect :: Options -> Shell (Either Line Line)
detect opt' = 
  inprocWithErr "ffmpeg" ["-i"
                 , opt' ^. in_file . packed
                 , "-map"
                 , "0:" <> opt' ^. voice_track . to show . packed
                 , "-filter:a"
                 , "silencedetect=noise=-30dB:d=0.5"
                 , "-f"
                 , "null"
                 , "-"
                 ] $ pure mempty
