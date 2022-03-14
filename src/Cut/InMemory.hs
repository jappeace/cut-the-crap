{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Links to ffmpeg-light allowing us to do analysis in memory.
--   this is a lot faster.
module Cut.InMemory
  ( readffmpeg
  )
where

import Control.Monad.Except
import UnliftIO.Exception
import qualified Codec.FFmpeg.Decode as Decode
import qualified Codec.FFmpeg.Encode as Encode
import Codec.FFmpeg.Encode(EncodingParams)
import Foreign.C.Types
import Codec.FFmpeg.Enums
import Data.Vector.Storable (Vector)
import Codec.FFmpeg.Types(AVFrame, InputSource(..))
import Codec.FFmpeg.Internal.Linear(V2(..))


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
  withWriter Encode.defaultParams imsOutFile $ \writer ->
  readFrames avPixFmtRgb32 imsInFile $ \(avframe, time) -> do
    pure ()

withWriter :: EncodingParams -> FilePath -> (
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
  res <- runExceptT $ Decode.frameReaderTime avPixFmtRgb32 (File path)
  case res of
    Left str -> error str
    Right (readCmd, cleanup) -> do
          let
            loop = do
              mFrame <- readCmd
              case mFrame  of
                Just frame -> (fun frame >> loop) `finally` cleanup
                Nothing -> pure ()
          loop
