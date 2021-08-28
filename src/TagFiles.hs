module TagFiles (tagFiles) where

-- Run a single FileParser, derived from a format string, against multiple
-- mp3 files, tagging them with eyeD3 flags.

import Prelude (String, (.), ($), (*>), show)
import Control.Arrow ((***), (+++), left)
import Control.Monad (mapM_)
import Control.Concurrent.Async (mapConcurrently_)
import Data.Either (Either, partitionEithers)
import Data.Function (const)
import Data.List (map, intercalate)
import Data.Tuple (uncurry)
import System.IO (IO, stderr, stdout)
import System.IO.Silently (hSilence)
import System.Posix.Escape.Unicode (escape)
import System.Process (system)
import Helpers ((◇), putStderr)
import Parser (FileParser(..))

parseFile ∷ FileParser → String → Either String String
parseFile f α = toError +++ toCmd $ (runParser f) α
  where toError = const ("Malformated file " ◇ α)
        toCmd ω = "eyeD3 " ◇ ω ◇ (escape α)

tagFiles ∷ FileParser → [String] → IO ()
tagFiles f =  run . (printErrors *** eyeD3) . partitionEithers . parse
  where parse       = map (parseFile f)
        printErrors = putStderr . intercalate "\n"
        eyeD3       = hSilence [stdout, stderr] . mapConcurrently_ system
        run         = uncurry (*>)
