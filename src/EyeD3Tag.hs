module EyeD3Tag where

-- The command `eyeD3` is used to tag files for now.
-- Each of its command-line flags has a corresponding instance in EyeD3Tag.

import Prelude (Show, String, ($), show)
import Helpers ((◇))

wrap ∷ String → String
wrap α = "\"" ◇ α ◇ "\""

data EyeD3Tag = Artist       String
              | AlbumArtist  String
              | Album        String
              | DiscNum      String
              | Genre        String
              | TrackNum     String
              | Title        String
              | Year         String

instance Show EyeD3Tag where
  show (Artist      α) = "-a " ◇ (show $ wrap α)
  show (AlbumArtist α) = "-b " ◇ (show $ wrap α)
  show (Album       α) = "-A " ◇ (show $ wrap α)
  show (DiscNum     α) = "-d " ◇ (show α)
  show (Genre       α) = "-G " ◇ (show $ wrap α)
  show (TrackNum    α) = "-n " ◇ (show α)
  show (Title       α) = "-t " ◇ (show $ wrap α)
  show (Year        α) = "-Y " ◇ (show α)

-- A newtype describing the constructor of an EyeD3Tag. Sits inside a Matcher
-- and is invoked on each filename to parse text into a proper ID3 tag.
newtype Tagger = Tagger { runTagger ∷ String → EyeD3Tag }

instance Show Tagger where
  show α = show $ (runTagger α) "x"
