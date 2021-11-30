{-# LANGUAGE RecursiveDo #-}

-- TODO write a test for this: https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads
-- | Deal with ctrl c events nicely.
--   don't just kill the main thread, kill every other registered thread as well.
-- https://ro-che.info/articles/2014-07-30-bracket
module CtrlC
  ( withKillThese
  , CSettings(..)
  , defSettings
  , forkTracked
  , track
  , untrack
  , installSignalHandlers
  , SignalException(..)
  )
where

import qualified Data.Set as Set
import Data.Foldable
import Control.Exception
import Data.Set(Set)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import System.Timeout
import Data.Typeable (Typeable)
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)
import Control.Monad(void)

data CState = MkCState {
  ccsTrackedThreads :: TVar (Set ThreadId) -- excluding the main thread
}
data CSettings = MkCSettings {
  csTimeout :: Int -- ^ in microseconds (1/10^6 seconds), prevents infinite blocking, smaller then 0 means no timeout
  }

defSettings :: CSettings
defSettings = MkCSettings 2000000

-- | this will fork out a thread that is already tracked by CState,
--   it also has the untrack handler attached.
forkTracked :: CState -> IO () -> IO ThreadId
forkTracked state io =
  mask $ \restore -> mdo -- if you want to fork..
    tid <- restore $ forkIO $ do -- restore the subthread
        io `finally` do
          atomically $ untrack state tid
    atomically $ track state tid -- but we need to not except here
    pure tid

track :: CState -> ThreadId -> STM ()
track state tid =
  modifyTVar tvar (Set.insert tid)
  where
    tvar = ccsTrackedThreads state

-- | This is used as adhoc cleanup as well.
--   all tracked threads should call this when they're done cleaning up
untrack :: CState -> ThreadId -> STM ()
untrack state tid =
  modifyTVar tvar (Set.delete tid)
  where
    tvar = ccsTrackedThreads state

-- | This should run on the main thread.
--
--   @
--   main :: IO ()
--   main = withKillThese $ \state -> yourProgram
--   @
--
--   state can then be used to fork out new threads that are watched.
withKillThese :: CSettings -> (CState -> IO ()) -> IO ()
withKillThese settings fun = do
  installSignalHandlers
  threads <- newTVarIO mempty
  mask $ \restore -> do
    restore (fun $ MkCState {
      ccsTrackedThreads = threads
      }) `finally` do
        threadSet <- readTVarIO threads
        traverse_ killThread threadSet
        void $ timeout (csTimeout settings) $ waitTillClean threads

waitTillClean :: TVar (Set ThreadId) -> IO ()
waitTillClean x = do
  curVal <- readTVarIO x
  if curVal == mempty then pure () else waitTillClean x

newtype SignalException = SignalException Signal
  deriving (Show, Typeable)
instance Exception SignalException

installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread_id <- myThreadId
  weak_tid <- mkWeakThreadId main_thread_id
  forM_ [ sigHUP, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ ] $ \sig ->
    installHandler sig (Catch $ send_exception weak_tid sig) Nothing
  where
    send_exception weak_tid sig = do
      m <- deRefWeak weak_tid
      case m of
        Nothing  -> return ()
        Just tid -> throwTo tid (toException $ SignalException sig)
