module EyeD3Tag (EyeD3Tag(..), Tagger(..), getTag) where

-- The command `eyeD3` is used to tag files for now.
-- Each of its command-line flags has a corresponding instance in EyeD3Tag.

import Prelude (($))
import Data.Text (Text, filter, pack, unpack)
import Helpers ((◇), (≠))

data EyeD3Tag = Artist       Text
              | AlbumArtist  Text
              | Album        Text
              | DiscNum      Text
              | Genre        Text
              | TrackNum     Text
              | Title        Text
              | Year         Text

show ∷ EyeD3Tag → Text
show (Artist      α) = "-a " ◇ α ◇ " "
show (AlbumArtist α) = "-b " ◇ α ◇ " "
show (Album       α) = "-A " ◇ α ◇ " "
show (DiscNum     α) = "-d " ◇ (filter (≠ '"') α) ◇ " "
show (Genre       α) = "-G " ◇ α ◇ " "
show (TrackNum    α) = "-n " ◇ (filter (≠ '"') α) ◇ " "
show (Title       α) = "-t " ◇ α ◇ " "
show (Year        α) = "-Y " ◇ (filter (≠ '"') α) ◇ " "


-- A newtype describing the constructor of an EyeD3Tag. Sits inside a FileParser
-- and is invoked on each filename to parse text into a proper ID3 tag.
newtype Tagger = Tagger { runTagger ∷ Text → EyeD3Tag }

getTag ∷ Tagger → Text → Text
getTag f α = show $ (runTagger f) α
