module Main where

import qualified Cut.Lib               as Lib
import           Cut.SpeechRecognition

main :: IO ()
main = do
  result <- speechAnalyses "goforward.raw"
  print result
  -- detect_words "heyo.raw"

-- fun :: ResultCode
-- fun = SUCCESS

