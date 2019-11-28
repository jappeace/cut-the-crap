module Cut.SplitVideo
  ( split
  )
where

import           Control.Lens
import           Cut.Ffmpeg
import           Cut.Options
import           Data.Text.Lens
import           Turtle                  hiding ( FilePath )

-- | Splits a video into segments
split :: FilePath -> Options -> Shell ()
split tmp opt' = do
  cp (fromText (tmp ^. packed))
     (fromText (opt' ^. out_file . packed <> "-full.mp4"))
  void $ ffmpeg
    [ "-i"
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
    ]
