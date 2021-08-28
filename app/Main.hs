module Main where

import Prelude (Either, IO, String, ($), (>>=), pure, putStrLn)
import Control.Arrow ((|||))
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.List (drop)
import System.Environment (getArgs)
import Arguments (Arguments(..), arguments)
import Helpers (head', putStderr)
import Parser (fileParser)
import TagFiles (tagFiles)

runProgram ∷ IO (Either String ())
runProgram = runExceptT $ do
  args   ← ExceptT arguments
  parser ← liftEither $ fileParser (format args)
  lift   $ tagFiles parser (files args)

main ∷ IO ()
main = runProgram >>= putStderr ||| pure
