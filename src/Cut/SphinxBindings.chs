{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}

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

instance Storable WordFrame where
        alignment _ = 4
        sizeOf _    = 16
        peek ptr    = do
          frame_from <- toInt <$> peekByteOff ptr 0
          print ("fromuh ", frame_from)
          frame_to <- toInt <$> peekByteOff ptr 4
          print ("touh ", frame_to)
          
          print "entereing the storable"
          ddd <- peekByteOff ptr 8
          print ("got a offset whatever", ddd)
          txt <- (peekCString ddd)
          print ("got a offset whatever", txt)
          pure $ WordFrame frame_from frame_to $ Text.pack txt
        poke ptr (WordFrame d c i) = do
            pokeByteOff ptr 0 d
            pokeByteOff ptr 4 c
            withCString (Text.unpack i) $ pokeByteOff ptr 8

frame_from :: Lens' WordFrame Int
frame_from = field @"_frame_from"

frame_to :: Lens' WordFrame Int
frame_to = field @"_frame_to"

frame_word :: Lens' WordFrame Text
frame_word = field @"_frame_word"

toInt :: CInt -> Int
toInt = fromInteger . toInteger

detectedToList :: CInt -> Ptr WordFrame -> IO [WordFrame]
detectedToList c_used frames  = do
  print "into detection list"
  print frames
  print "found words used"
  print "attempting to arr"
  print (frames, used)
  print $ plusPtr frames $ sizeOf (undefined :: WordFrame) * 1
  print $ plusPtr frames $ sizeOf (undefined :: WordFrame) * 2
  traverse (peek . plusPtr frames . (*) (sizeOf (undefined :: WordFrame)) ) [0..(used - 1)]
  where
    used = toInt c_used
-- | given a raw audio file, we can detect words
speechAnalyses :: FilePath -> IO (Either ResultCode [WordFrame])
speechAnalyses filePath = do
  print filePath
  c_result <- withCString filePath detect_words_ffi
  print ("got result", c_result)
  c_words <- peekByteOff c_result 8 
  c_words_used <- peekByteOff c_result 16 
  print ("found words", c_words)
  status_code <- toEnum . fromIntegral <$> {#get detect_result.code #} c_result
  case status_code of
    Success -> Right <$> detectedToList c_words_used c_words
    x -> pure $ Left x
    
-- (worduh_framuh 16), (word_frame* 8),
-- (int 4), (detect_results 24),
-- (detected_words 16), (result_code 4)

foreign import ccall "detect_words" detect_words_ffi :: CString -> IO ({#type detect_result #}) 

-- type DetectResult = {#type detect_result #}

-- {#fun detect_words as detectWords {`String'} -> `DetectResult' #}
