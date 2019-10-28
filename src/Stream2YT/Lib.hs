{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Stream2YT.Lib
  ( main
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Options.Applicative
import           Options.Generic
import           Stream2YT.Jumpcutter
import           Stream2YT.Options
import           Stream2YT.SplitVideo
import           System.IO.Temp
import qualified Turtle                  as Sh

main :: (MonadMask m, MonadUnliftIO m) => m ()
main = do
  set'' <- liftIO readSettings
  withTempDirectory "/tmp" "streamedit" $ \temp -> do
      Sh.sh $ jump temp set''
      Sh.sh $ split temp set''
  -- lgr  <- newLogger Debug stdout
  -- env  <- newEnv <&> (envLogger .~ lgr) .
  --         (envScopes .~ youTubeUploadScope)
  -- runResourceT $ runGoogle env $ send $ videosInsert "id" $
  --   video & vStatus . _Just . vsPrivacyStatus ?~ Private
  -- pure ()

readSettings :: IO (Options)
readSettings =
  customExecParser (prefs showHelpOnError) $
  info
    parseRecord
    (fullDesc <> Options.Applicative.header "Webservice" <>
     progDesc "Run the API")
