module Format (normalize, replace) where

import Prelude (String, (.), otherwise)
import Data.Char (Char, toLower)
import Data.List (drop, intercalate, map, take)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)
import Helpers ((⊙), (◇), fork)

lower ∷ Set String
lower = fromList ["A", "An", "And", "By", "In", "On", "Of", "At", "With", "The",
                  "For", "From", "Into", "Unto", "To", "As"]

checkCaps ∷ String → String
checkCaps α
  | member α lower = toLower ⊙ α
  | otherwise      = α

onWords ∷ ([String] → [String]) → String → String
onWords f = intercalate " " . f . splitOn " "

normalize ∷ String → String
normalize = onWords (fork (◇) (take 1) (map checkCaps . drop 1))

replace ∷ String → String → String
replace α= intercalate " " . splitOn α
