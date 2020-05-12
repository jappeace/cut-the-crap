{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Cut.FFI
  (detect_words
  )
where

import           Foreign.Storable

-- data ResultCode = SUCCESS |
--                  FAILED_CONFIG_OBJECT |
--                  FAILED_CREATE_RECOGNIZER |
--                  FAILED_UNABLE_INPUTFILE


data DetectResult
  deriving (Storable)

-- | Splits a video into segments
foreign import ccall "detect_words" detect_words :: String -> IO DetectResult
