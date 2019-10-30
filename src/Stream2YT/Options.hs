{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Stream2YT.Options
  ( Options,
    in_vid, out_file, seg_size, silent_treshold, silent_speed, sound_speed, frame_margin
  ) where

import           Control.Lens
import           Data.Generics.Product.Fields
import           Options.Generic

data Options = Options
  { inFile   :: FilePath <?> "The input video"
  , outFile :: FilePath <?> "The output name without format"
  , segmentSize :: (Maybe Int) <?> "The size of video segments in minutes"
  , silentTreshold :: (Maybe Float) <?> "The treshold for determining intersting sections"
  , frameMargin :: (Maybe Int) <?> "Margin in frames around interesting sections"
  , soundSpeed :: (Maybe Float) <?> "The speed of sounded sections"
  , silentSpeed :: (Maybe Float) <?> "THe speed of silent sections"
  } deriving (Show, Generic)

halp :: Iso' (a <?> b) a
halp = iso unHelpful Helpful

in_vid :: Lens' Options FilePath
in_vid = field @"inFile" . halp

out_file :: Lens' Options FilePath
out_file = field @"outFile" . halp

seg_size :: Lens' Options Int
seg_size = field @"segmentSize" . halp . non 20

frame_margin :: Lens' Options Int
frame_margin = field @"frameMargin" . halp . non 1

silent_treshold :: Lens' Options Float
silent_treshold = field @"silentTreshold" . halp . non 0.03

sound_speed :: Lens' Options Float
sound_speed = field @"soundSpeed" . halp . non 1.00

silent_speed :: Lens' Options Float
silent_speed = field @"silentSpeed" . halp . non 5.00

instance ParseRecord Options
