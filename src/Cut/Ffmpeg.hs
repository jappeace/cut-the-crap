module Cut.Ffmpeg
  ( ffmpeg
  , floatToText
  )
where

import qualified Data.Text                     as Text
import           Numeric
import           Turtle                  hiding ( FilePath )

-- | Wrap ffmpeg for convenience and logging
ffmpeg :: [Text] -> Shell (Either Line Line)
ffmpeg args = do
  liftIO $ putStr "Running: "
  liftIO $ print $ "ffmpeg " <> Text.unwords args
  inprocWithErr "ffmpeg" args $ pure mempty

-- | Format floats for cli
floatToText :: Double -> Text
floatToText = Text.pack . flip (showFFloat (Just 10)) ""
