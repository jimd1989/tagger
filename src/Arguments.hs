module Arguments where

import Prelude (Either, IO, String, (.), show)
import Control.Applicative (liftA2)
import Control.Arrow ((+++))
import Control.Error.Util (note)
import Data.Function (const)
import Data.List (drop)
import Data.List.NonEmpty (NonEmpty, map, nonEmpty)
import Data.Text (Text, pack)
import System.Environment (getArgs)
import Helpers ((◁), (⊙), fork, head')

data Arguments = Arguments {
  format ∷ Text,
  files ∷ NonEmpty Text
}

makeFormatString ∷ [String] → Either String Text
makeFormatString = (const "Format string not provided" +++ pack) . head'

makeFiles ∷ [String] → Either String (NonEmpty Text)
makeFiles = map pack ◁ note "No audio files provided" . nonEmpty . drop 1

arguments ∷ IO (Either String Arguments)
arguments = (fork (liftA2 Arguments) makeFormatString makeFiles) ⊙ getArgs
