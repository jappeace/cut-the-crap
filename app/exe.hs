module Main where

import           Control.Lens
import           Cut.Analyze
import qualified Cut.Lib               as Lib
import           Cut.Options
import           Cut.SpeechRecognition
import           Data.Foldable
import qualified Data.Text.IO          as T
import           Shelly                hiding (FilePath)

txt :: String
txt = "input"

main :: IO ()
main = do
  result <- shelly $ detectSpeech (set voice_track 1 simpleOptions) "tmptmp" (txt <>".mkv")

  traverse_ (T.writeFile (txt <> ".srt") . Lib.makeSrt) result
  -- detect_words "heyo.raw"

-- fun :: ResultCode
-- fun = SUCCESS
