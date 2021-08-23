module Matcher where

-- Matchers are parsed from a single user-supplied format string.
-- They describe how to parse a section of filename text into an ID3 tag.
-- One list of Matchers is run against every filename, with the expectation
-- that each name follows the same format.

import Prelude (Char, Eq, Int, Maybe, Show, String)
import EyeD3Tag (Tagger)

newtype Delimeter = Delimeter { getDelimeter ∷ Maybe Char }
  deriving (Eq, Show)

data Matcher = ExactText String
             | Until Tagger Delimeter Int
             | Number Tagger Delimeter
  deriving Show
