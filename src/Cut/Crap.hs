{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is where it all started
module Cut.Crap
  ( entryPoint
  , combineDir
  , makeSrt
  , runListenCut
  , runEdit
  , runGenSubs
  )
where

import           Cut.Shell
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Cut.Analyze
import           Cut.CutVideo
import           Cut.Options
import           Cut.SpeechRecognition
import           Data.Foldable                (fold, foldl', traverse_)
import           Data.Generics.Product.Fields
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as T
import           Data.Text.Lens
import           Data.Time
import           GHC.Generics                 hiding (to)
import           Options.Applicative
import           Shelly                       hiding (FilePath)
import           System.IO.Temp
import Network.URI(URI)
import System.Random
import Data.Word

runYoutubeDL :: FileIO a -> URI -> IO FilePath
runYoutubeDL opts x = do

  inputNumbers :: Word32 <- randomIO
  let inputChars :: String
      -- youtube-dl doesn't do .mkv (not supported)
      inputChars = (show inputNumbers) <> ".mp4"
      filePath :: String
      filePath = fromMaybe inputChars $ opts ^. work_dir
  void $ shelly $ youtube_dl x filePath
  pure filePath



downloadIfNeccisary :: FileIO InputSource -> IO (FileIO FilePath)
downloadIfNeccisary x = do
  result <- sequence $ (runYoutube <|> alreadyLocal)
  case result of
    Nothing -> error $ "Couldn't find " <> show x
    Just y -> pure $ x & in_file .~ y
  where
    runYoutube :: Maybe (IO FilePath)
    runYoutube = x ^? in_file . input_src_remote . to (runYoutubeDL x)

    alreadyLocal :: Maybe (IO FilePath)
    alreadyLocal = preview (in_file . input_src_local_file . to pure) x

downloadCutifNeccisary :: ListenCutOptionsT InputSource -> IO (ListenCutOptionsT FilePath)
downloadCutifNeccisary cut = repackRes
      where
        downloadRes :: IO (FileIO FilePath)
        downloadRes =  downloadIfNeccisary $ cut ^. lc_fileio
        repackRes :: IO (ListenCutOptionsT (FilePath))
        repackRes = downloadRes <&> \y -> lc_fileio .~ y $ cut

-- | reads settings from terminal and runs whatever command was
--   given in program options
entryPoint :: MonadMask m => MonadUnliftIO m => m ()
entryPoint = do
  result <- liftIO readSettings
  betterResult <-  liftIO $ case  result of
    ListenCut cut -> ListenCut <$> downloadCutifNeccisary cut
    GenerateSubtitles x -> GenerateSubtitles <$> downloadIfNeccisary x

  -- I'm mr meeseeks look at me!
  sequence_ $ betterResult ^? listen_cut_prism . to (void . runListenCut)
           <|> betterResult ^? gnerate_sub_prism . to runGenSubs
  pure ()

runGenSubs :: MonadIO m => FileIO FilePath -> m ()
runGenSubs options = liftIO $ withTempDir options $ \tmp -> do
    betterOptions <- downloadCutifNeccisary simpleOptions
    result <- shelly $ detectSpeech (set voice_track 1 betterOptions) tmp $ options ^. in_file
    print result
    traverse_ (T.writeFile (options ^. out_file) . makeSrt) result

-- | Runs cut-the-crap with provided `ListenCutOptions`
runListenCut :: MonadMask m => MonadUnliftIO m => ListenCutOptions -> m [Interval Sound]
runListenCut options = do
  liftIO $ putStr "started with options: "
  liftIO $ print options

  -- first figure out what's up in the vid
  parsed <- detectSoundInterval options

  -- then do stuff to it
  parsed <$ case parsed of
    [] ->
      liftIO
        $ putStr
            "\n\nNo silence in input video detected. There is nothing to be cut so exiting.\n\n"
    _ -> liftIO $ withTempDir (options ^. lc_fileio) $ runEdit options parsed

withTempDir :: FileIO FilePath -> (FilePath -> IO ()) -> IO ()
withTempDir filioOpts fun =
  maybe (withTempDirectory "/tmp" "streamedit" fun) fun $ filioOpts ^. work_dir

-- | Run editing on video from options with preprovided detections
--   normally aquired throug `detect`
runEdit :: ListenCutOptions -> [Interval Sound] -> FilePath -> IO ()
runEdit options parsed tempDir = do
  extract options tempDir parsed
  shelly $ combineDir options tempDir
  getMusic options tempDir

combineDir :: ListenCutOptions -> FilePath -> Sh ()
combineDir _ tempDir = do
  res <- lsT $ fromText $ Text.pack (tempDir <> extractDir)
  let paths = Text.unlines $ flip (<>) "'" . ("file '" <>) <$> res
  writefile (fromText $ Text.pack $ tempDir <> "/input.txt") paths
  combine tempDir

readSettings :: IO (ProgramOptions InputSource)
readSettings = customExecParser (prefs showHelpOnError) $ info
  (parseProgram <**> helper)
  (fullDesc <> header "Cut the crap" <> progDesc
    "Automated video extracting, can cut out silences"
  )

musicFile :: FilePath
musicFile = "music.mp3"

withMusicFile :: FilePath
withMusicFile = "combined.mkv"

getMusic :: ListenCutOptions -> FilePath -> IO ()
getMusic opt' tempDir = do
  res <- case opt' ^. music_track of
    Nothing -> pure $ Text.pack combinedFile
    Just x  -> do
      shelly $ extractMusicTrack x (opt' ^. lc_fileio . in_file) tempDir
      shelly $ mergeMusicAndVideo tempDir
      pure $ Text.pack (tempDir <> "/" <> withMusicFile)
  putStrLn "done get music"
  shelly $ cp (fromText res) (opt' ^. lc_fileio . out_file . packed . to fromText)
  pure ()
  where combinedFile = tempDir <> "/" <> combineOutput

extractMusicTrack :: Int -> FilePath -> FilePath -> Sh ()
extractMusicTrack musicTrack inputFile tempDir = void $ ffmpeg inputFile args
 where -- https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
  args =
    [ "-map"
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


data SrtDisplay = SrtDisplay
  { _srt_from     :: DiffTime
  , _srt_to       :: DiffTime
  , _srt_words    :: Text
  , _srt_position :: Int
  } deriving (Show, Eq, Generic)

instance Semigroup SrtDisplay where
  (<>) a b = SrtDisplay{
    _srt_from = a ^. srt_from,
    _srt_to = b ^. srt_to,
    _srt_words = a ^. srt_words <> " " <> b ^. srt_words,
    _srt_position = a ^. srt_position
    }
instance Monoid SrtDisplay where
  mempty = SrtDisplay{
    _srt_from = 0,
    _srt_to = 0,
    _srt_words = mempty,
    _srt_position = 0
    }

srt_from :: Lens' SrtDisplay DiffTime
srt_from = field @"_srt_from"
srt_to :: Lens' SrtDisplay DiffTime
srt_to = field @"_srt_to"
srt_words :: Lens' SrtDisplay Text
srt_words = field @"_srt_words"
srt_position :: Lens' SrtDisplay Int
srt_position = field @"_srt_position"

makeSrt :: [WordFrame] -> Text.Text
makeSrt frames = fold $ fmap (formatSrt . foldl' (<>) mempty) $ groupBySentence $ imap (toSrtDisplay off) frames
  where off = fromMaybe noOffset $ frames ^? ix 0 . frame_from

groupBySentence :: [SrtDisplay] -> [[SrtDisplay]]
groupBySentence = snd . foldl' innerFold ([], []) -- face

innerFold :: ([SrtDisplay], [[SrtDisplay]]) -> SrtDisplay ->  ([SrtDisplay], [[SrtDisplay]])
innerFold (prev, res) x = if x ^. srt_words == "<sil>" then ([], prev : res) else
  (x : prev, res)


toSrtDisplay :: FrameOffset -> Int -> WordFrame -> SrtDisplay
toSrtDisplay firstOffset ix' frame = SrtDisplay
  { _srt_from     = frame ^. frame_from . to (toDiffTime firstOffset)
  , _srt_to       = frame ^. frame_to . to (toDiffTime firstOffset)
  , _srt_words    = frame ^. frame_word
  , _srt_position = ix'
  }


-- | wikipedia explains the srt format pretty well: https://en.wikipedia.org/wiki/SubRip
--  in escence :
-- [A numeric counter identifying each sequential subtitle]
-- [The time that the subtitle should appear on the screen] --–> [d the time it should disappear]
-- [Subtitle text itself on one or more lines]
-- [A blank line containing no text, indicating the end of this subtitle]
formatSrt :: SrtDisplay -> Text.Text
formatSrt sentence = fold
  [ sentence ^. srt_position . to show . packed
  , "\n"
  , Text.pack
  $  formatTime defaultTimeLocale "%0H:%0M:%0S,000"
  $  sentence
  ^. srt_from
  , " --> "
  , Text.pack
  $  formatTime defaultTimeLocale "%0H:%0M:%0S,000"
  $  sentence
  ^. srt_to
  , "\n"
  , (sentence ^. srt_words)
  , "\n"
  , "\n"
  ]
