module Main where

import Math.NumberTheory.Pell.Test.Reduced (prop_reduced)
import Math.NumberTheory.Pell.Test.Utils (test)

main :: IO ()
main = do
    test prop_reduced