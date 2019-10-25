module Stream2YT.SplitVideo
  ( split
  ) where

import           Control.Lens
import qualified Data.Text         as T
import           Data.Text.Lens
import           Stream2YT.Options
import           Turtle

split :: Options -> Shell ()
split opt = do
  procs "ffmpeg" ["-i"
                 , opt ^. in_vid . packed
                 , "-c"
                 , "copy"
                 , "-map"
                 , "0"
                 , "-segment_time"
                 , "00:" <> (opt ^. seg_size . packed) <> ":00"
                 , "-f"
                 , "segment"
                 , "-reset_timestamps"
                 , "1"
                 , opt ^. out_file . packed <> "%03d.mp4"
                 ] $ pure mempty
