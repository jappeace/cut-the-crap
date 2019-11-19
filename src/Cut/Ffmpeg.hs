module Cut.Ffmpeg
  ( ffmpeg
  ) where

import qualified Data.Text as Text
import           Turtle    hiding (FilePath)

-- | Wrap ffmpeg for convenience and logging
ffmpeg :: [Text] -> Shell (Either Line Line)
ffmpeg args = do
  liftIO $ putStr "Running: "
  liftIO $ print $ "ffmpeg " <> Text.unwords args
  inprocWithErr "ffmpeg" args $ pure mempty
