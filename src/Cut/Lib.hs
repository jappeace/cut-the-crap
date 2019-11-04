{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cut.Lib
  ( main
  ) where

import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Control.Lens
import Control.Foldl
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Cut.Options
import           Cut.SplitVideo
import           Options.Applicative
import           Options.Generic
import           System.IO.Temp
import qualified Turtle                  as Sh

main :: (MonadMask m, MonadUnliftIO m) => m ()
main = do
  set'' <- liftIO readSettings

  -- TODO filter on regex silencedetect,
  -- then group by or on silence end + duration
  -- groupBy
  -- prefilter
  lines <- Sh.fold (detect set'') list 
  liftIO $ putStrLn "nothing"
  liftIO $
    Text.putStr $ Text.unlines $ (Prelude.either (
                                 ("err" <>) . Sh.lineToText) (("std" <> ) . Sh.lineToText))  <$> lines
  -- withTempDirectory "/tmp" "streamedit" $ \temp -> do
  --     Sh.sh $ split temp set''

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
