{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cut.Lib
  ( entryPoint
  , combineDir
  , makeSrt
  )
where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Cut.Analyze
import           Cut.CutVideo
import           Cut.Ffmpeg
import           Cut.Options
import           Cut.SpeechRecognition
import           Data.Foldable                (fold)
import           Data.Generics.Product.Fields
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Lens
import           Data.Time
import           GHC.Generics                 hiding (to)
import           Options.Applicative
import           Shelly                       hiding (FilePath)
import           System.IO.Temp

entryPoint :: (MonadMask m, MonadUnliftIO m) => m ()
entryPoint = catch main
  $ \exec -> liftIO (print (exceptionString, exec :: SomeException))

exceptionString :: String
exceptionString = "Uncaught exception: "

main :: (MonadMask m, MonadUnliftIO m) => m ()
main = do
  options <- liftIO readSettings
  liftIO $ putStr "started with options: "
  liftIO $ print options

  parsed <- detectSoundInterval options
  case parsed of
    [] ->
      liftIO
        $ putStr
            "\n\nNo silence in input video detected. There is nothing to be cut so exiting.\n\n"
    _ -> case options ^. work_dir of
      Nothing ->
        withTempDirectory "/tmp" "streamedit" $ liftIO . runEdit options parsed
      Just x -> liftIO $ runEdit options parsed x

runEdit :: Options -> [Interval Sound] -> FilePath -> IO ()
runEdit options parsed tempDir = do
  extract options tempDir parsed
  shelly $ combineDir options tempDir
  getMusic options tempDir

combineDir :: Options -> FilePath -> Sh ()
combineDir _ tempDir = do
  res <- lsT $ fromText $ Text.pack tempDir
  let paths = Text.unlines $ flip (<>) "'" . ("file '" <>) <$> res
  writefile (fromText $ Text.pack $ tempDir <> "/input.txt") paths
  combine tempDir

readSettings :: IO Options
readSettings = customExecParser (prefs showHelpOnError) $ info
  parseRecord
  (fullDesc <> Options.Applicative.header "Cut the crap" <> progDesc
    "Automated video extracting, can cut out silences"
  )

musicFile :: FilePath
musicFile = "music.mp3"

withMusicFile :: FilePath
withMusicFile = "combined.mkv"

getMusic :: Options -> FilePath -> IO ()
getMusic opt' tempDir = do
  res <- case opt' ^. music_track of
    Nothing -> pure $ Text.pack combinedFile
    Just x  -> do
      shelly $ extractMusicTrack x (opt' ^. in_file) tempDir
      shelly $ mergeMusicAndVideo tempDir
      pure $ Text.pack (tempDir <> "/" <> withMusicFile)
  putStrLn "done get music"
  shelly $ cp (fromText res) (opt' ^. out_file . packed . to fromText)
  pure ()
  where combinedFile = tempDir <> "/" <> combineOutput

extractMusicTrack :: Int -> FilePath -> FilePath -> Sh ()
extractMusicTrack musicTrack inputFile tempDir = void $ ffmpeg inputFile args
 where -- https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
  args =
    ["-map"
    , "0:" <> Text.pack (show musicTrack)
    , Text.pack (tempDir <> "/" <> musicFile)
    ]

mergeMusicAndVideo :: FilePath -> Sh ()
mergeMusicAndVideo tempDir = void $ ffmpeg' args
 where -- https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
  args =
    [ "-i"
    , Text.pack $ tempDir <> "/" <> combineOutput
    , "-i"
    , Text.pack $ tempDir <> "/" <> musicFile
    , "-filter_complex"
    , "[0:a][1:a]amerge=inputs=2[a]"
    , "-map"
    , "0:v"
    , "-map"
    , "[a]"
    , "-c:v"
    , "copy"
    , "-c:a"
    , "mp3"
    , "-ac"
    , "2"
    , "-shortest"
    , Text.pack (tempDir <> "/" <> withMusicFile)
    ]


data SrtSentence = SrtSentence
  { _srt_from     :: DiffTime
  , _srt_to       :: DiffTime
  , _srt_words    :: Text
  , _srt_position :: Int
  } deriving (Show, Eq, Generic)

srt_from     :: Lens' SrtSentence DiffTime
srt_from = field @"_srt_from"
srt_to       :: Lens' SrtSentence DiffTime
srt_to = field @"_srt_to"
srt_words    :: Lens' SrtSentence Text
srt_words = field @"_srt_words"
srt_position :: Lens' SrtSentence Int
srt_position = field @"_srt_position"

makeSrt :: [WordFrame] -> Text.Text
makeSrt frames = fold $ imap (fmap formatSrt . (toSrtSentence off)) frames
  where
    off = fromMaybe noOffset $ frames ^? ix 0 . frame_from

toSrtSentence :: FrameOffset -> Int -> WordFrame -> SrtSentence
toSrtSentence firstOffset ix' frame = SrtSentence
  { _srt_from = frame ^. frame_from . to (toDiffTime firstOffset )
  , _srt_to = frame ^. frame_to . to (toDiffTime firstOffset )
  , _srt_words = frame ^. frame_word
  , _srt_position = ix'
  }


-- | wikipedia explains the srt format pretty well: https://en.wikipedia.org/wiki/SubRip
--  in escence :
-- [A numeric counter identifying each sequential subtitle]
-- [The time that the subtitle should appear on the screen] --–> [d the time it should disappear]
-- [Subtitle text itself on one or more lines]
-- [A blank line containing no text, indicating the end of this subtitle]
formatSrt :: SrtSentence -> Text.Text
formatSrt sentence = fold [
  sentence ^. srt_position . to show . packed,
  "\n",
  Text.pack $ formatTime defaultTimeLocale "%0H:%0M:%0S,000" $ sentence ^. srt_from,
  " --> ",
  Text.pack $ formatTime defaultTimeLocale "%0H:%0M:%0S,000" $ sentence ^. srt_to,
  "\n",
  (sentence ^. srt_words),  "\n", "\n"]

