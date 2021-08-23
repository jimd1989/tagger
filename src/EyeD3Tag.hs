module EyeD3Tag where

-- The command `eyeD3` is used to tag files for now.
-- Each of its command-line flags has a corresponding instance in EyeD3Tag.

import Prelude (Show, String, ($), show)
import Data.List (filter)
import Helpers ((◇), (≠))

data EyeD3Tag = Artist       String
              | AlbumArtist  String
              | Album        String
              | DiscNum      String
              | Genre        String
              | TrackNum     String
              | Title        String
              | Year         String

instance Show EyeD3Tag where
  show (Artist      α) = "-a " ◇ (show α) ◇ " "
  show (AlbumArtist α) = "-b " ◇ (show α) ◇ " "
  show (Album       α) = "-A " ◇ (show α) ◇ " "
  show (DiscNum     α) = "-d " ◇ (filter (≠ '"') $ show α) ◇ " "
  show (Genre       α) = "-G " ◇ (show α) ◇ " "
  show (TrackNum    α) = "-n " ◇ (filter (≠ '"') $ show α) ◇ " "
  show (Title       α) = "-t " ◇ (show α) ◇ " "
  show (Year        α) = "-Y " ◇ (filter (≠ '"') $ show α) ◇ " "

-- A newtype describing the constructor of an EyeD3Tag. Sits inside a Matcher
-- and is invoked on each filename to parse text into a proper ID3 tag.
newtype Tagger = Tagger { runTagger ∷ String → EyeD3Tag }

instance Show Tagger where
  show α = show $ (runTagger α) "x"
