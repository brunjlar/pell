module Math.NumberTheory.Pell.Test.Solve (
    prop_isSolution) where

import Control.Monad (liftM2)
import Math.NumberTheory.Pell (solve)
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import Test.QuickCheck

data Problem = Problem { d :: Integer, n :: Integer } deriving (Show, Eq)

isProperD :: Integral a => a -> Bool
isProperD n = (n > 0) && not (isSquare n)

genD :: Gen Integer
genD = scale (* 2) $ suchThat arbitrary isProperD

shrinkD :: Integer -> [Integer]
shrinkD d = filter isProperD $ shrink d

genN :: Gen Integer
genN = scale (* 2) $ sized genN' where
    genN' 0 = elements [-1, 1]
    genN' 1 = elements [-4, 4]
    genN' n = do
        x <- suchThat arbitrary (> 0)
        let y = integerSquareRoot x
        elements [-y, y]
        
shrinkN :: Integer -> [Integer]
shrinkN n = filter (/= 0) $ shrink n
    
instance Arbitrary Problem where
    arbitrary = liftM2 Problem genD genN
    shrink (Problem d n) = [Problem d' n | d' <- shrinkD d] ++ [Problem d n' | n' <- shrinkN n]
    
prop_isSolution :: Problem -> Property
prop_isSolution (Problem d n) =
    classify (n ==   1)                  "n ==  1"     $
    classify (n == (-1))                 "n == -1"     $ 
    classify (n ==   4)                  "n ==  4"     $
    classify (n == (-4))                 "n == -4"     $
    classify (abs n `notElem` [1, 4]) "|n| /= 1, 4" $
    classify (n^2 < d)                   "n^2 < d"     $
    classify (n^2 > d)                   "n^2 > d"     $
    classify (d <= 100)                  "d <= 100"    $
    classify (d >  100)                  "d >  100"    $
    (n^2 < d) ==> all (\(x, y) -> x^2 - d * y^2 == n) $ take 20 $ solve d n
