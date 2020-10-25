{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Deal with downloading and the cli options involved
module Cut.Download
  ( downloadIfNeccisary
  , downloadCutifNeccisary
  )
where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Cut.Options
import           Cut.Shell
import           Data.Foldable          (traverse_)
import           Data.Word
import           Network.URI            (URI)
import           Options.Applicative
import           Shelly                 hiding (FilePath)
import           System.Random

-- | Downloads a URI to the filepath returned
runYoutubeDL :: FileIO a -> URI -> IO FilePath
runYoutubeDL opts x = do
  -- we need to make our own because youtube-dl doesn't write to existing files
  -- System.IO.Temp would've worked neatly here.
  inputNumbers :: Word32 <- randomIO
  let inputChars :: String
      -- youtube-dl doesn't do .mkv (not supported)
      inputChars = show inputNumbers
      filePath :: String
      filePath = maybe inputChars (flip (</>) inputChars) $ opts ^. work_dir
      resultPath = filePath <> ".mkv"
  shelly $ do
    void $ youtube_dl x filePath
    -- (#52): fix bug where youtube-dl doesn't always create .mkv file output
    out <- test_f filePath
    when out $ mv filePath resultPath
  pure resultPath -- because 'youtube_dl' sets merge-output-format

aquireFilePath :: FileIO InputSource -> IO (FileIO FilePath)
aquireFilePath x = do
  result <- sequence $ (runYoutube <|> alreadyLocal)
  case result of
    Nothing -> error $ "Couldn't find " <> show x
    Just y  -> pure $ x & in_file .~ y
  where
    runYoutube :: Maybe (IO FilePath)
    runYoutube = x ^? in_file . input_src_remote . to (runYoutubeDL x)

    alreadyLocal :: Maybe (IO FilePath)
    alreadyLocal = preview (in_file . input_src_local_file . to pure) x

removeDownloaded :: FileIO InputSource -> FileIO FilePath -> IO ()
removeDownloaded cliInput aquiredPath =
  traverse_ (const $  -- only do this if we got it from remote (we did the download)
               traverse_ (shelly . rm) $ aquiredPath ^? in_file
               ) $ cliInput ^? in_file . input_src_remote

-- | 'FileIO' can have a remote, if so we download that and pass the downloaded
--   path to the continuation, if not we simply pass the input path to
--   the continuation.
--   We need a continuation to clean up the downloaded file.
downloadIfNeccisary :: MonadMask m => MonadIO m => FileIO InputSource -> (FileIO FilePath -> m a) -> m a
downloadIfNeccisary x = bracket (liftIO $ aquireFilePath x) (liftIO . removeDownloaded x)

-- | this does the same as 'downloadIfNeccisary' but makes it work for
--   'ListenCutOptionsT' with some type weaving
downloadCutifNeccisary :: MonadMask m => MonadIO m => ListenCutOptionsT InputSource -> ((ListenCutOptionsT FilePath) -> m a) -> m a
downloadCutifNeccisary cut fun =
  downloadIfNeccisary (cut ^. lc_fileio) $
    fun . (\y -> lc_fileio .~ y $ cut)
