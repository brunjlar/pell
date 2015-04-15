module Math.NumberTheory.Moduli.SquareRoots ( sqrts ) where

sqrts :: Integer -> Integer -> [Integer]
sqrts a m = [x | x <- [0 .. (m - 1)], (x * x - a) `mod` m == 0]
