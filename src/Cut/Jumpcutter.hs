module Cut.Jumpcutter
  ( jump, outName
  ) where

import           Control.Lens
import           Cut.Options
import qualified Data.Text      as T
import           Data.Text.Lens
import           Turtle         hiding (FilePath)

--    ./jumpcutter
--  --input_file /tmp/place-input004.mp4
  --temp_folder /tmp/TEMP47 --frame_margin 2
outName :: T.Text
outName = "jumpout.mp4"

jump :: FilePath -> Options -> Shell ()
jump tmp opt' =
  procs "jumpcutter" ["--input_file"
                 , opt' ^. in_vid . packed
                 , "--temp_folder"
                 , tmp ^. packed <> "/special"
                 , "--frame_margin"
                 , opt' ^. frame_margin . to show . packed
                 , "--sounded_speed"
                 , opt' ^. sound_speed . to show . packed
                 , "--silent_speed"
                 , opt' ^. silent_speed . to show . packed
                 , "--output_file"
                 , (tmp ^. packed) <> outName
                 , "--silent_threshold"
                 , opt' ^. silent_treshold . to show . packed
                 ] $ pure mempty


