module Math.NumberTheory.Moduli.SquareRoots.Test (
    prop_sqrts) where

import Math.NumberTheory.Moduli.SquareRoots (sqrts)
import Test.QuickCheck

newtype Problem = Problem (Integer, Integer) deriving (Show, Eq)

instance Arbitrary Problem where
    
    arbitrary = do
        m <- suchThat arbitrary (> 0)
        a <- choose (0, m - 1)
        return $ Problem (a, m)

    shrink (Problem (a, m)) =
        [Problem (a', m) | a' <- shrink a, a' >= 0] ++
        [Problem (a, m') | m' <- shrink m, m > a]

naive :: Integer -> Integer -> [Integer]
naive a m = [x | x <- [0 .. (m - 1)], (x * x - a) `mod` m == 0]

prop_sqrts :: Problem -> Property
prop_sqrts (Problem (a, m)) = (naive a m) === (sqrts a m) 
