module Main where

import Math.NumberTheory.Moduli.SquareRoots.Test (prop_sqrts)
import Math.NumberTheory.Pell.Test.Reduced (prop_reduced)
import Math.NumberTheory.Pell.Test.Solve (prop_solves)
import Math.NumberTheory.Pell.Test.Utils (test)

main :: IO ()
main = do
    test prop_sqrts
    test prop_reduced
    test (prop_solves 1000000)
