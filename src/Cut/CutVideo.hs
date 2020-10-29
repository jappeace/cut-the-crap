{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Extract sounded parts, and combine again
module Cut.CutVideo
  ( extract
  , Interval(..)
  , Silent
  , Sound
  , combine
  , combineOutput
  , combineDir
  )
where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Cut.Analyze
import           Cut.Shell
import           Cut.Options
import           Data.Foldable
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Lens
import           Shelly              hiding (FilePath, shelly)
import           Text.Printf         (printf)

-- | convert a sounded interval into a filename where we write the temporary extracted file in
toFileName :: ListenCutOptions -> FilePath -> Interval Sound -> FilePath
toFileName options tmp inter = tmp
         </> getOutFileName options
         <> "-"
         <> fname
         <> ".mkv"
  where
      fname    = printf "%010d" (truncate $ interval_start inter * 100 :: Integer)

toArgs :: ListenCutOptions -> FilePath -> Interval Sound -> (Interval Sound, [Text])
toArgs options tmp inter =
  ( inter
  , -- keep ter interval for debugging
    ["-y", "-ss", start, "-t", duration, "-i", options ^. lc_fileio . in_file . packed]
    <> specifyTracks options
    <> [ Text.pack $ toFileName options tmp inter
       ]
  )
 where
  start    = floatToText $ interval_start inter
  duration = floatToText $ interval_duration inter

extractDir :: FilePath
extractDir = "extract"

extract :: ListenCutOptions -> FilePath -> [Interval Sound] -> IO ()
extract options tempDir intervals = do
  shelly $ mkdir_p exdir
  traverse_
      (\(inter, args) -> void $ catch (shelly $ ffmpeg' args) $ \exec -> do
        liftIO
          (print ("expection during edit: ", exec :: SomeException, args, inter)
          )
        pure ["expection"]
      )
    $   toArgs options exdir <$> intervals
  liftIO $ putStrLn "finished extracting"

  where
    exdir = tempDir </> extractDir

combineOutput :: FilePath
combineOutput = "combined-output.mkv"

combine :: FilePath -> Sh ()
combine tempDir = do
  output' <- ffmpeg' args
  liftIO $ print ("output", output')
 where -- https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
  args =
    [ "-y"
    , "-f"
    , "concat"
    , "-safe"
    , "0"
    , "-i"
    , Text.pack (tempDir <> "/input.txt")
    , "-c"
    , "copy"
    , Text.pack $ tempDir </> combineOutput
    ]

combineDir :: ListenCutOptions -> FilePath -> [Interval Sound] -> Sh ()
combineDir options tempDir intervals = do
  liftIO $ putStrLn "start combining the dir"
  let paths = Text.unlines $ flip (<>) "'" . ("file '" <>) <$> res
  writefile (fromText $ Text.pack $ tempDir <> "/input.txt") paths
  combine tempDir
  where
    res = Text.pack . toFileName options exdir <$> intervals
    -- for relative dirs ffmpeg doesn't accept prepending of tempdir
    exdir = if hasWorkDir then extractDir else tempDir </> extractDir
    hasWorkDir :: Bool
    hasWorkDir = has (lc_fileio . work_dir . _Just) options
