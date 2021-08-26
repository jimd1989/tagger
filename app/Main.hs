module Main where

import Prelude (Either, IO, String, ($), (>>=), pure, putStrLn)
import Control.Arrow ((|||))
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.List (drop)
import System.Environment (getArgs)
import Helpers (head', putStderr)
import Parser (fileParser)
import TagFiles (tagFiles)

runProgram ∷ IO (Either String ())
runProgram = runExceptT $ do
  args         ← lift getArgs
  formatString ← liftEither $ head' args
  files        ← pure $ drop 1 args
  parser       ← liftEither $ fileParser formatString 
  lift         $ tagFiles parser files

main ∷ IO ()
main = runProgram >>= putStderr ||| pure
