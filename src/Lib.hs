module Lib
  ( libF
  ) where

import           Control.Monad.IO.Class

libF :: MonadIO m => m ()
libF = liftIO $ print "hello, world"
