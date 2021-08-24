module Arguments (Arguments(..), arguments) where

import Prelude (Either, IO, Show, String, (.), pure)
import Control.Applicative (liftA2)
import Data.List (drop)
import System.Environment (getArgs)
import Helpers ((⊙), fork, head')

data Arguments = Arguments { 
  formatString ∷ String,
  files ∷ [String]
} deriving Show

arguments ∷ IO (Either String Arguments)
arguments = (fork makeArguments getFormatString getFiles) ⊙ getArgs
  where makeArguments   = liftA2 Arguments
        getFormatString = head'
        getFiles        = pure . drop 1
