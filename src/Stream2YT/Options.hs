{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Stream2YT.Options
  ( Options(..),
    in_vid, out_file, seg_size
  ) where

import           Control.Lens
import           Data.Generics.Product.Fields
import           Options.Generic

data Options = Options
  { in_vid_field   :: FilePath <?> "The input video"
  , out_file_field :: FilePath <?> "The output name without format"
  , seg_size_field :: String <?> "The size of video segments in minutes"
  } deriving (Show, Generic)

halp :: Iso' (a <?> b) a
halp = iso unHelpful Helpful

in_vid :: Lens' Options FilePath
in_vid = field @"in_vid_field" . halp
out_file :: Lens' Options FilePath
out_file = field @"out_file_field" . halp
seg_size :: Lens' Options String
seg_size = field @"seg_size_field" . halp

instance ParseRecord Options
