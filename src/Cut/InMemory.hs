{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Links to ffmpeg-light allowing us to do analysis in memory.
--   this is a lot faster.
module Cut.InMemory
  ( readffmpeg
  , defaultSettings
  , InMemSettings(..)
  )
where

import qualified Cut.InMemory.Tagged as Tagged
import Control.Monad.Except
import UnliftIO.Exception
import qualified Codec.FFmpeg.Decode as Decode
import qualified Codec.FFmpeg.Encode as Encode
import qualified Codec.FFmpeg.Juicy as Juicy
import Codec.FFmpeg.Encode(EncodingParams)
import Foreign.C.Types
import Codec.FFmpeg.Enums
import Data.Vector.Storable (Vector)
import Codec.FFmpeg.Types(AVFrame, InputSource(..), getPixelFormat)
import Codec.FFmpeg.Internal.Linear(V2(..))
import Codec.FFmpeg(initFFmpeg,  setLogLevel)
import qualified Codec.FFmpeg.Common as Common



data InMemSettings = MkMemSettings
  { imsInFile :: FilePath
  , imsOutFile :: FilePath
  , imsWidth  :: CInt
  , imsHeight :: CInt
  }

defaultSettings :: InMemSettings
defaultSettings = MkMemSettings
  { imsInFile = "in.mp4"
  , imsOutFile = "out.mkv"
  , imsWidth  = 1080
  , imsHeight = 1920
  }

readffmpeg :: InMemSettings -> IO ()
readffmpeg MkMemSettings{..} = do
  initFFmpeg
  setLogLevel avLogInfo
  cleanup <- withWriter (Encode.defaultParams imsWidth imsHeight) imsOutFile $ \writer -> do
    readFrames imsInFile $ \(avframe, time) -> do
      pxfmt <-  getPixelFormat avframe
      vec <- runExceptT $ Tagged.frameToVectorExceptT avframe
      pxfmt <- getPixelFormat avframe
      case vec of
        Right v -> writer (pxfmt,V2 imsWidth imsHeight,v)
        Left errs -> error $ "oh noes, died at " <> show (time, errs) <> " , for some reason, maybeT squashed everything :(  "
  cleanup

withWriter :: EncodingParams -> FilePath ->
  (
    (( AVPixelFormat
    , V2 CInt -- ^ resolution
    , Vector CUChar -- ^ pixel data
    ) -> IO ()) -> IO a
  ) -> IO a
withWriter params path fun = do
  bracket (Encode.videoWriter params path)
          (\writer -> writer Nothing) $ \writer -> (fun (writer . Just))

-- | steps trough all frames and performs cleanup
readFrames ::  FilePath -> ((AVFrame, Double) -> IO ()) -> IO (IO ())
readFrames path fun = do
  res <- runExceptT $ Decode.frameReaderTime avPixFmtRgba (File path)
  case res of
    Left str -> error str
    Right (readCmd, cleanup) -> do
          let
            loop = do
              mFrame <- readCmd
              case mFrame  of
                Just frame -> (fun frame >> loop)
                Nothing -> pure ()
          cleanup <$ loop
