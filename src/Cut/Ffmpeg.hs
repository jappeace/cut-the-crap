module Cut.Ffmpeg
  ( ffmpeg
  , ffmpeg'
  , floatToText
  )
where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Numeric
import           Shelly

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
