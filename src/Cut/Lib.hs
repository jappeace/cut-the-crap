{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -w #-}

module Cut.Lib
  ( entryPoint, getStart , getEnd , getDuration
  ) where

import qualified Control.Foldl           as Fl
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
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

data SilentDetect = SilentDetect
  { silence_start    :: Double
  , silence_end      :: Double
  , silence_duration :: Double
  } deriving Show

parse :: (Sh.Line, Sh.Line) -> SilentDetect
parse x = SilentDetect {
   silence_start    = getStart $ fst x
  , silence_end      = getEnd $ snd x
  , silence_duration = getDuration $ snd x
  }

getStart :: Sh.Line -> Double
getStart line = read $ match ^. _3
  where
  str = Text.unpack $ Sh.lineToText line
  match :: (String, String, String)
  match = str =~ startMatch

startMatch :: String
startMatch = "(.*)?: "

pipe :: String
pipe = " \\| "

getDuration :: Sh.Line -> Double
getDuration line = read $ match2 ^. _1
  where
  str = Text.unpack $ Sh.lineToText line
  match1 :: (String, String, String)
  match1 = str =~ startMatch
  match2 :: (String, String, String)
  match2 = (match1 ^. _3) =~ pipe

getEnd :: Sh.Line -> Double
getEnd line = read $ match2 ^. _3
  where
  str = Text.unpack $ Sh.lineToText line
  match1 :: (String, String, String)
  match1 = str =~ pipe
  match2 :: (String, String, String)
  match2 = (match1 ^. _1) =~ startMatch

entryPoint :: (MonadMask m, MonadUnliftIO m) => m ()
entryPoint = do
  set'' <- liftIO readSettings

  -- TODO filter on regex silencedetect,
  -- then group by or on silence end + duration
  -- groupBy
  -- prefilter
  lines' <- Sh.fold (detect set'') $ Fl.prefilter (either (("[silencedetect" ==) . Text.take (Text.length "[silencedetect") . Sh.lineToText) (const False)) Fl.list
  let
      linedUp :: [(Sh.Line, Sh.Line)]
      linedUp = do
        elem <- imap (\i a -> (i, a)) $ zip (take (length lines' - 1) lines') (drop 1 lines')
        if even (fst elem) then
          pure $ (bimap (fromLeft mempty) (fromLeft mempty) $ snd elem)
        else empty
      parsed = parse <$> linedUp

  liftIO $ print parsed

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
