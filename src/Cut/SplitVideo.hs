module Cut.SplitVideo
  ( split
  ) where

import           Control.Lens
import           Cut.Jumpcutter
import           Cut.Options
import           Data.Text.Lens
import           Turtle         hiding (FilePath)

split :: FilePath -> Options -> Shell ()
split tmp opt' = do
  cp (fromText $ (tmp ^. packed) <> outName) (fromText (opt' ^. out_file . packed <> "-full.mp4"))
  procs "ffmpeg" ["-i"
                 , (tmp ^. packed) <> outName
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
