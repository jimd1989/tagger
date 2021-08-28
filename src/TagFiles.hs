module TagFiles (tagFiles) where

-- Run a single FileParser, derived from a format string, against multiple
-- mp3 files, tagging them with eyeD3 flags.

import Prelude (String, (.), ($), (*>), show)
import Control.Arrow ((***), (+++))
import Control.Concurrent.Async (mapConcurrently_)
import Data.Either (Either, partitionEithers)
import Data.Function (const)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, map, toList)
import Data.Text (Text, unpack)
import Data.Tuple (uncurry)
import System.IO (IO, stderr, stdout)
import System.IO.Silently (hSilence)
import System.Process (system)
import Helpers ((◇), fork, putStderr)
import Parser (FileParser(..))

parseFile ∷ FileParser → Text → Either String String
parseFile f α = toError +++ toCmd $ (runParser f) α
  where toError = const ("Malformated file " ◇ (unpack α))
        toCmd ω = "eyeD3 " ◇ (unpack ω) ◇ (show $ unpack α)

tagFiles ∷ FileParser → NonEmpty Text → IO ()
tagFiles f =  run . (printErrors *** eyeD3) . partitionEithers . parse
  where parse       = toList . map (parseFile f)
        printErrors = putStderr . intercalate "\n"
        eyeD3       = hSilence [stderr, stdout] . mapConcurrently_ system
        run         = uncurry (*>)
