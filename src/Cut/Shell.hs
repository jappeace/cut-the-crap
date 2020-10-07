
-- | Run shell programs
module Cut.Shell
  ( ffmpeg
  , ffmpeg'
  , floatToText
  , youtube_dl
  )
where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Numeric
import           Shelly
import Network.URI(URI)

-- | Wrap ffmpeg for convenience and logging
--  technically supports multiple inputs but for convenice we threw that.
ffmpeg :: Prelude.FilePath -> [Text] -> Sh [Text]
ffmpeg file args = ffmpeg' ("-y" : "-i" : Text.pack file : args)

ffmpeg' :: [Text] -> Sh [Text]
ffmpeg' args = do
  liftIO $ putStr "Running: "
  liftIO $ print $ "ffmpeg " <> Text.unwords args
  run_ "ffmpeg" args
  Text.lines <$> lastStderr

-- | Format floats for cli
floatToText :: Double -> Text
floatToText = Text.pack . flip (showFFloat (Just 10)) ""

youtube_dl :: URI -> FilePath -> Sh [Text]
youtube_dl uri path' = do
  liftIO $ print $ "running youtube-dl " <> Text.unwords args
  run_ "youtube-dl" args
  Text.lines <$> lastStderr
  where
    args = [Text.pack $ show uri,
           "-o", Text.pack path']
