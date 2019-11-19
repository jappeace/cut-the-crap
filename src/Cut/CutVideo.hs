{-# OPTIONS_GHC -Wno-type-defaults #-}

module Cut.CutVideo
  ( edit
  , Interval(..)
  , Silent
  , Sound
  , detectSound
  , combine
  )
where

import qualified Control.Foldl       as Fl
import           Control.Lens
import           Control.Monad.Catch
import           Cut.Options
import           Data.Foldable
import qualified Data.Text           as Text
import           Data.Text.Lens
import           Turtle              hiding (FilePath)
import qualified Turtle              as T

data Silent
data Sound

data Interval e = Interval
  { interval_start    :: Double
  , interval_end      :: Double
  , interval_duration :: Double
  } deriving Show

detectSound :: [Interval Silent] -> [Interval Sound]
detectSound = filter ((0 <) . interval_end) . reverse . snd . foldl'
  (flip compare')
  ((Interval 0 0 0, []))

compare'
  :: Interval Silent
  -> (Interval Silent, [Interval Sound])
  -> (Interval Silent, [Interval Sound])
compare' x' y = (x', soundedInterval : snd y)
 where
  soundedInterval = Interval { interval_start    = interval_end $ fst y
                             , interval_end      = interval_start x'
                             , interval_duration = soundEnd - soundStart - 0.02
                             }
  soundEnd   = interval_start x'
  soundStart = interval_end $ fst y


toArgs :: Options -> FilePath -> Interval Sound -> [Text]
toArgs opt' tmp inter =
  [ "-y"
  , "-ss"
  , start
  , "-t"
  , duration
  , "-i"
  , opt' ^. in_file . packed
  , Text.pack tmp
    <> "/"
    <> opt' ^. out_file .  packed
    <> start
    <> "-"
    <> duration
    <> ".mpg"
  , "-qscale:v"
  , "1"
  ]
 where
  duration = Text.pack $ show $ interval_duration inter
  start    = Text.pack $ show $ interval_start inter

edit :: Options -> FilePath -> [Interval Sound] -> IO ()
edit opt' tempDir intervals = do
  liftIO $ print intervals
  traverse_
      (\args -> do
        liftIO $ print args
        out <- catch (T.fold (inprocWithErr "ffmpeg" args $ pure mempty) Fl.list) $ \exec -> do
          liftIO (print ("expection during edit: ", exec :: SomeException, args))
          pure [Left "expection"]
        liftIO $ print ("finisehd with ", out)
      )
    $   toArgs opt' tempDir
    <$> intervals

  liftIO $ putStrLn "donee"

combine :: Options -> [T.FilePath] -> Shell ()
combine opt' tempfiles = do
  liftIO $ print ("arguments", args, tempfiles)
  output' <- inprocWithErr
        "ffmpeg"
        args
    $ pure mempty
  liftIO $ print ("output", output')
 where
  paths = encodeString <$> tempfiles
  expr  = drop 1 $ foldr (\y x' -> x' <> "|" <> y) "" paths
  args = [ "-y"
        , "-i"
        , "concat:" <> (Text.pack $ expr)
        , opt' ^. out_file . packed <> ".mp4"
        ]
