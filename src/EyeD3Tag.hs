module EyeD3Tag where

import Prelude (Int, Show, String, show)
import Helpers ((◇))

data EyeD3Tag = Artist       String
              | AlbumArtist  String
              | Album        String
              | DiscNum      String
              | Genre        String
              | TrackNum     String
              | Title        String
              | Year         String

instance Show EyeD3Tag where
  show (Artist      α) = "-a " ◇ (show α)
  show (AlbumArtist α) = "-b " ◇ (show α)
  show (Album       α) = "-A " ◇ (show α)
  show (DiscNum     α) = "-d " ◇ (show α)
  show (Genre       α) = "-G " ◇ (show α)
  show (TrackNum    α) = "-n " ◇ (show α)
  show (Title       α) = "-t " ◇ (show α)
  show (Year        α) = "-Y " ◇ (show α)

-- A newtype describing the constructor of an EyeD3Tag. Sits inside a Matcher.
newtype Tagger = Tagger { runTagger ∷ String → EyeD3Tag }

instance Show Tagger where
  show α = "λ α → EyeD3Tag α"
