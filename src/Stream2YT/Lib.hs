{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Stream2YT.Lib
  ( main
  ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Network.Google
import           Network.Google.YouTube
import           Options.Applicative
import           Options.Generic
import           Stream2YT.Options
import           Stream2YT.SplitVideo
import           System.IO               (stdout)
import qualified Turtle                  as Sh

main :: (MonadCatch m, MonadIO m, MonadUnliftIO m) => m ()
main = do
  -- Sh.sh . split =<< liftIO readSettings
  lgr  <- newLogger Debug stdout
  env  <- newEnv <&> (envLogger .~ lgr) .
          (envScopes .~ youTubeUploadScope)
  runResourceT $ runGoogle env $ send $ videosInsert "id" $
    video & vStatus . _Just . vsPrivacyStatus ?~ Private
  pure ()

readSettings :: IO (Options)
readSettings =
  customExecParser (prefs showHelpOnError) $
  info
    parseRecord
    (fullDesc <> Options.Applicative.header "Webservice" <>
     progDesc "Run the API")
