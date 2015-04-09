module Main where

import Math.NumberTheory.Pell.Test.IntegerD
import Math.NumberTheory.Pell.Test.Reduced (prop_reduced)
import Math.NumberTheory.Pell.Test.Solve (prop_isSolution)
import Math.NumberTheory.Pell.Test.Utils (test)

main :: IO ()
main = do
    test prop_reduced
    test prop_isSolution
