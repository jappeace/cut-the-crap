module Main where

import           Cut.Analyze
import qualified Cut.Lib               as Lib
import           Cut.Options
import           Cut.SpeechRecognition
import           Data.Foldable
import qualified Data.Text.IO          as T
import           Shelly                hiding (FilePath)

main :: IO ()
main = do
  result <- shelly $ detectSpeech simpleOptions "tmptmp" "sample.mkv"

  traverse_ (T.writeFile "sample.srt" . Lib.makeSrt) result
  -- detect_words "heyo.raw"

-- fun :: ResultCode
-- fun = SUCCESS
