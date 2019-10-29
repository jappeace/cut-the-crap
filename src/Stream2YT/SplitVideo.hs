module Stream2YT.SplitVideo
  ( split
  ) where

import           Control.Lens
import           Data.Text.Lens
import           Stream2YT.Jumpcutter
import           Stream2YT.Options
import           Turtle               hiding (FilePath)

split :: FilePath -> Options -> Shell ()
split tmp opt' = do
  procs "ffmpeg" ["-i"
                 , (tmp ^. packed) <> outName
                 , "-c"
                 , "copy"
                 , "-map"
                 , "0"
                 , "-segment_time"
                 , "00:" <> (opt' ^. seg_size . packed) <> ":00"
                 , "-f"
                 , "segment"
                 , "-reset_timestamps"
                 , "1"
                 , opt' ^. out_file . packed <> "%03d.mp4"
                 ] $ pure mempty
