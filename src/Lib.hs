module Lib
  ( libF
  ) where

import           Control.Monad.IO.Class

libF :: MonadIO m => m ()
libF = liftIO $ putStrLn "hello, world"
