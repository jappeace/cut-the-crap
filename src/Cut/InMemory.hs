{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Links to ffmpeg-light allowing us to do analysis in memory.
--   this is a lot faster.
module Cut.InMemory
  ( readffmpeg
  )
where

import qualified Codec.FFmpeg.Decode as Decode
import qualified Codec.FFmpeg.Encode as Encode

data InMemSettings = MkMemSettings
  { imsInFile :: FilePath
  , imsOutFile :: FilePath
  , imsWidth  :: CInt
  , imsHeight :: CInt
  }

defaultSettings :: InMemSettings
defaultSettings = MkMemSettings
  { imsInFile = "in.mkv"
  , imsOutFile = "out.mkv"
  , imsWidth  = 1080
  , imsHeight = 1920
  }

readFfmpeg :: InMemSettings -> IO ()
readFfmpeg MkMemSettings{..} =
  withWriter defaultParams imsOutFile $ \writer ->
  readFrames avPixFmtRgb32 imsInFile $ \(avframe, time) -> do
    pure ()

withWriter ::  EncodingParams -> FilePath -> (
  ( AVPixelFormat
  , V2 CInt -- ^ resolution
  , Vector CUChar -- ^ pixel data
  ) -> IO ()
  ) -> IO ()
withWriter params path fun = do
  bracket (Encode.frameWriter params path)
          (\writer -> writer Nothing) $ fun . Just

-- | steps trough all frames and performs cleanup
readFrames ::  AVPixelFormat -> FilePath -> ((AVFrame, Double) -> IO ()) -> IO ()
readFrames pxfmt path fun = do
  (readCmd, cleanup) <- Decode.frameReader avPixFmtRgb32 (File imsInFile)
  loop
  where
    loop = do
      mFrame <- readCmd
      case mFrame  of
        Just frame -> (fun frame `finally` cleanup) >> loop
        Nothing -> pure ()
