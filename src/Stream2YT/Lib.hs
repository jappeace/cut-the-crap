{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Stream2YT.Lib
  ( main
  ) where

import           Control.Monad.IO.Class
import           Options.Applicative
import           Options.Generic
import           Stream2YT.Options
import           Stream2YT.SplitVideo
import qualified Turtle                 as Sh

main :: MonadIO m => m ()
main = do
  Sh.sh . split =<< liftIO readSettings

readSettings :: IO (Options)
readSettings =
  customExecParser (prefs showHelpOnError) $
  info
    parseRecord
    (fullDesc <> Options.Applicative.header "Webservice" <>
     progDesc "Run the API")
