{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Reimplementation of ffmpeg-light that doesn't squash errors
module Cut.InMemory.Tagged
  ( frameToVectorExceptT
  , FrameVecErrors(..)
  )
where

import Codec.Picture
import Codec.FFmpeg.Common
import Codec.FFmpeg.Decode
import Codec.FFmpeg.Encode
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Internal.Linear (V2(..))
import Codec.FFmpeg.Types
import Control.Arrow (first)
import Control.Monad ((>=>))
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.Types
import Foreign.Storable (sizeOf)
import Data.Maybe (maybe)

data FrameVecErrors = FrameBufferFailed
  deriving Show

frameToVectorExceptT :: AVFrame -> ExceptT FrameVecErrors IO (V.Vector CUChar)
frameToVectorExceptT frame = do

  bufSize <- maybeToExceptT FrameBufferFailed $ fromIntegral <$> frameBufferSizeT frame

  v <- liftIO $ VM.new bufSize
  liftIO $ VM.unsafeWith v (frameCopyToBuffer frame)
  liftIO $ V.unsafeFreeze v

