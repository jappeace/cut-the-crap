{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -w #-}

module Cut.Lib
  ( entryPoint, combineDir
  ) where

import qualified Control.Foldl           as Fl
import           Control.Lens
import           Control.Monad           (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Cut.Analyze
import           Cut.Analyze
import           Cut.CutVideo
import           Cut.Options
import           Cut.SplitVideo
import           Data.Bifunctor
import           Data.Either
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Options.Applicative
import           Options.Generic
import           System.IO.Temp
import           Text.Regex.TDFA         hiding (empty)
import qualified Turtle                  as Sh


entryPoint :: (MonadMask m, MonadUnliftIO m) => m ()
entryPoint = catch main $ \exec -> do
    liftIO (print ("Uncaught exception: ", exec :: SomeException))

main :: (MonadMask m, MonadUnliftIO m) => m ()
main = do
  set'' <- liftIO readSettings
  liftIO $ putStr "started with options: "
  liftIO $ print set''

  -- TODO filter on regex silencedetect,
  -- then group by or on silence end + duration
  -- groupBy
  -- prefilter
  parsed <- detect set''

  withTempDirectory "/tmp" "streamedit" $ \temp -> do
      liftIO $ (edit set'' temp parsed)
      Sh.testdir "/tmp/tomp" >>= flip when (Sh.rmtree "/tmp/tomp")
      Sh.cptree (Sh.decodeString temp) "/tmp/tomp"
      liftIO $ combineDir set'' temp
  pure ()
  -- withTempDirectory "/tmp" "streamedit" $ \temp -> do
  --     Sh.sh $ split temp set''

  -- lgr  <- newLogger Debug stdout
  -- env  <- newEnv <&> (envLogger .~ lgr) .
  --         (envScopes .~ youTubeUploadScope)
  -- runResourceT $ runGoogle env $ send $ videosInsert "id" $
  --   video & vStatus . _Just . vsPrivacyStatus ?~ Private
  -- pure ()

combineDir :: Options -> FilePath -> IO ()
combineDir set'' temp = do
      res <- Sh.fold (Sh.ls $ Sh.decodeString temp) Fl.list
      let paths :: Text
          paths = Text.unlines $ reverse $ Text.pack . (flip (<>) "'") . ("file '" <>) . Sh.encodeString <$> res

      Text.putStrLn paths

      Sh.writeTextFile (Sh.decodeString inputsPath)  paths
      Sh.sh (combine set'' inputsPath)
      where
        inputsPath = temp <> "/" <> "input.txt"

readSettings :: IO (Options)
readSettings =
  customExecParser (prefs showHelpOnError) $
  info
    parseRecord
    (fullDesc <> Options.Applicative.header "Cut the crap" <>
     progDesc "Automated video editing, can cut out silences")
