module Main where

import Math.NumberTheory.Pell.Test.IntegerD (prop_plus, prop_minus, prop_times, prop_norm_multiplicative)
import Math.NumberTheory.Pell.Test.Reduced (prop_reduced)
import Math.NumberTheory.Pell.Test.Solve (prop_solves)
import Math.NumberTheory.Pell.Test.Utils (test)

main :: IO ()
main = do
    test prop_plus
    test prop_minus
    test prop_times
    test prop_norm_multiplicative
    test prop_reduced
    test (prop_solves 1000000)
