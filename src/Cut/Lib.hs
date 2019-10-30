{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cut.Lib
  ( main
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Cut.Jumpcutter
import           Cut.Options
import           Cut.SplitVideo
import           Options.Applicative
import           Options.Generic
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
    (fullDesc <> Options.Applicative.header "Cut the crap" <>
     progDesc "Automated video editing, can cut out silences")
