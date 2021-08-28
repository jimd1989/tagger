module EyeD3Tag (EyeD3Tag(..), Tagger(..), getTag) where

-- The command `eyeD3` is used to tag files for now.
-- Each of its command-line flags has a corresponding instance in EyeD3Tag.

import Prelude (Show, String, ($), show)
import System.Posix.Escape.Unicode (escape)
import Capitalization (normalize)
import Helpers ((◇))

data EyeD3Tag = Artist       String
              | AlbumArtist  String
              | Album        String
              | DiscNum      String
              | Genre        String
              | TrackNum     String
              | Title        String
              | Year         String

-- This is stupid: revisit
-- Just don't use `show` on Strings
instance Show EyeD3Tag where
  show (Artist      α) = "-a " ◇ (escape $ normalize α) ◇ " "
  show (AlbumArtist α) = "-b " ◇ (escape $ normalize α) ◇ " "
  show (Album       α) = "-A " ◇ (escape $ normalize α) ◇ " "
  show (DiscNum     α) = "-d " ◇ α ◇ " "
  show (Genre       α) = "-G " ◇ (escape $ normalize α) ◇ " "
  show (TrackNum    α) = "-n " ◇ α ◇ " "
  show (Title       α) = "-t " ◇ (escape $ normalize α) ◇ " "
  show (Year        α) = "-Y " ◇ α ◇ " "

-- A newtype describing the constructor of an EyeD3Tag. Sits inside a FileParser
-- and is invoked on each filename to parse text into a proper ID3 tag.
newtype Tagger = Tagger { runTagger ∷ String → EyeD3Tag }

instance Show Tagger where
  show α = show $ (runTagger α) "x"

getTag ∷ Tagger → String → String
getTag f α = show $ (runTagger f) α
