
-- | Run shell programs
--
--   We mostly add a lot of logging to figure out what goes on
module Cut.Shell
  ( ffmpeg
  , ffmpeg'
  , floatToText
  , youtube_dl
  , shelly
  )
where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Numeric
import           Shelly(Sh)
import qualified Shelly as Sh
import Network.URI(URI)
import Text.Printf
import Data.Time
import System.IO(stdout, hFlush)
import Control.Monad.IO.Class

-- | Wrap ffmpeg for convenience and logging
--  technically supports multiple inputs but for convenice we threw that.
ffmpeg :: Prelude.FilePath -> [Text] -> Sh [Text]
ffmpeg file args = ffmpeg' ("-y" : "-i" : Text.pack file : args)

ffmpeg' :: [Text] -> Sh [Text]
ffmpeg' = run "ffmpeg"

-- | Format floats for cli
floatToText :: Double -> Text
floatToText = Text.pack . flip (showFFloat (Just 10)) ""

-- | Wrapper for shelley that always flushes stdout
shelly :: MonadIO m => Sh a -> m a
shelly x = do
  res <- Sh.shelly x
  liftIO $ hFlush stdout -- flush stdout per command run
  pure res

run :: FilePath -> [Text] -> Sh [Text]
run command args = do
  time' <- liftIO getCurrentTime
  let format = formatTime defaultTimeLocale "%F %T" time'
  liftIO $ putStrLn "--- "
  liftIO $ printf "%s: %s %s" format command (Text.unwords args)
  liftIO $ putStrLn "   " -- flush
  Sh.run_ command args
  liftIO $ putStrLn "--- "
  result <- Text.lines <$> Sh.lastStderr
  pure result

youtube_dl :: URI -> FilePath -> Sh [Text]
youtube_dl uri path' = run "youtube-dl" args
  where
    args = [Text.pack $ show uri
           , "-o", Text.pack path'
           , "--merge-output-format", "mkv"
           ]
