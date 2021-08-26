module Main where

import Prelude (Either, IO, String, ($), (>>=), pure, putStrLn)
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.List (drop)
import System.Environment (getArgs)
import Helpers ((⊙), head')
import Parser (FileParser(..), fileParser)

runProgram ∷ IO (Either String ())
runProgram = runExceptT $ do
  args         ← lift getArgs
  formatString ← liftEither $ head' args
  files        ← pure $ drop 1 args
  parser       ← liftEither $ fileParser formatString 
  pure () 

main ∷ IO ()
main = putStrLn "x"
