{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cut.Lib
  ( main
  ) where

import Data.Either
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Control.Lens
import qualified Control.Foldl as Fl
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Cut.Options
import           Cut.SplitVideo
import           Options.Applicative
import           Options.Generic
import           System.IO.Temp
import qualified Turtle                  as Sh
import Data.Bifunctor

data SilentDetect = SilentDetect
  { silence_start :: Double
  , silence_end :: Double
  , silence_duration :: Double
  }

main :: (MonadMask m, MonadUnliftIO m) => m ()
main = do
  set'' <- liftIO readSettings

  -- TODO filter on regex silencedetect,
  -- then group by or on silence end + duration
  -- groupBy
  -- prefilter
  lines <- Sh.fold (detect set'') $ Fl.prefilter (either (("[silencedetect" ==) . Text.take (Text.length "[silencedetect") . Sh.lineToText) (const False)) Fl.list
  let
      linedUp :: [(Sh.Line, Sh.Line)]
      linedUp = do
        elem <- imap (\i a -> (i, a)) $ zip (take (length lines - 1) lines) (drop 1 lines)
        if even (fst elem) then
          pure $ (bimap (fromLeft mempty) (fromLeft mempty) $ snd elem)
        else empty
  liftIO $ print linedUp
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
