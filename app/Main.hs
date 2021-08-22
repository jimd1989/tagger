module Main where

import Protolude (IO, putStrLn)
import Data.Text (Text, unpack)

main ∷ IO ()
main = putStrLn (unpack x)

x ∷ Text
x = "x"
