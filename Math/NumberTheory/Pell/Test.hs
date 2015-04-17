module Main where

import Math.NumberTheory.Moduli.SquareRoots.Test (prop_sqrtsPP, prop_sqrts)
import Math.NumberTheory.Pell.Test.Reduced (prop_reduced)
import Math.NumberTheory.Pell.Test.Solve (Problem (..), prop_solves)
import Math.NumberTheory.Pell.Test.Utils (test)

main :: IO ()
main = do
    test prop_sqrtsPP
    test prop_sqrts
    test prop_reduced
    test (prop_solves 100 $ Problem 7   9)
    test (prop_solves 100 $ Problem 5 (-4))
    test (prop_solves 100 $ Problem 2 (-7))
    test (prop_solves 100000)
