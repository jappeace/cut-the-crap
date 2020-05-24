{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TypeApplications #-}

-- | This entire module leaks like crazy, it's not a big deal because
--   at the moment this process is confined
module Cut.SphinxBindings(speechAnalyses) where

import           GHC.Generics
import Data.Text(Text)
import qualified Data.Text as Text
import Control.Monad (liftM, when)
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Lens
import           Data.Generics.Product.Fields

#include "speech_recognition.h"

{#enum result_code as ResultCode {underscoreToCase}#}
deriving instance Show ResultCode 

data WordFrame = WordFrame
  { _frame_from :: Int
  , _frame_to :: Int
  , _frame_word :: Text
  } deriving (Generic, Show)

frame_from :: Lens' WordFrame Int
frame_from = field @"_frame_from"

frame_to :: Lens' WordFrame Int
frame_to = field @"_frame_to"

frame_word :: Lens' WordFrame Text
frame_word = field @"_frame_word"

data DetectResults = DetectResults -- TODO this as an Either?
  { _results_code :: ResultCode
  , _results_words :: [WordFrame]
} deriving (Generic, Show)

results_code :: Lens' DetectResults ResultCode
results_code = field @"_results_code"

results_words :: Lens' DetectResults [WordFrame]
results_words = field @"_results_words"

getFrame :: ({#type word_frame #}) -> IO WordFrame
getFrame framuh = do
  _frame_from <- toInt <$> {#get word_frame.from_frame #} framuh
  _frame_to <- toInt <$> {#get word_frame.to_frame #} framuh
  _word_string <- peekCString <$> {#get word_frame.word #} framuh
  _frame_word <- Text.pack <$> _word_string
  pure $ WordFrame {..}


toInt :: CInt -> Int
toInt = fromInteger . toInteger

detectedToList :: {#type detected_words #} -> IO [WordFrame]
detectedToList detected_ptr = do
  print "into detection list"
  print detected_ptr
  c_used <-  {#get detected_words.used #} detected_ptr
  print "found words used"
  frames <- {#get detected_words.frames #} detected_ptr
  print "found framus"
  let used = toInt c_used
  traverse getFrame =<< peekArray used frames

-- | given a raw audio file, we can detect words
speechAnalyses :: FilePath -> IO (Either ResultCode [WordFrame])
speechAnalyses filePath = do
  print filePath
  c_filePath <- newCString filePath
  print c_filePath
  let c_result = detect_words_ffi c_filePath
  print "got result"
  print c_result
  c_words <- {#get detect_result.words #} c_result
  print "found words"
  print c_words
  status_code <- toEnum . fromIntegral <$> {#get detect_result.code #} c_result
  case status_code of
    Success -> Right <$> detectedToList c_words
    x -> pure $ Left x

foreign import ccall "detect_words" detect_words_ffi :: CString -> {#type detect_result#}


