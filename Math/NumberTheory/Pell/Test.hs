module Math.NumberTheory.Pell.Test where

import Distribution.TestSuite.QuickCheck (Test, testProperty, testGroup)
import Math.NumberTheory.Moduli.SquareRoots.Test (prop_sqrtsPP, prop_sqrts)
import Math.NumberTheory.Pell.Test.Reduced (prop_reduced)
import Math.NumberTheory.Pell.Test.Solve (Problem (..), prop_solves)

tests :: IO [Test]
tests = return 
            [
                testGroup "SquareRoots"
                    [
                        testProperty "sqrtsPP"       prop_sqrtsPP,
                        testProperty "sqrts"         prop_sqrts
                    ],
                testGroup "Pell"
                    [
                        testProperty "reduced"       prop_reduced,
                        testProperty "solves 7   9"  (prop_solves 100 $ Problem 7   9),
                        testProperty "solves 5 (-4)" (prop_solves 100 $ Problem 5 (-4)),
                        testProperty "solves 2 (-7)" (prop_solves 100 $ Problem 2 (-7)),
                        testProperty "solves"        (prop_solves 100000)
                    ]
           ]

-- main :: IO ()
-- main = do
--     test prop_sqrtsPP
--     test prop_sqrts
--     test prop_reduced
--     test (prop_solves 100 $ Problem 7   9)
--     test (prop_solves 100 $ Problem 5 (-4))
--     test (prop_solves 100 $ Problem 2 (-7))
--     test (prop_solves 100000)
--
