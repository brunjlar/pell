module Main where

import Math.NumberTheory.Pell
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import System.Exit (exitFailure)
import Test.QuickCheck

main :: IO ()
main = do
    test prop_reduced
        
test :: Testable p => p -> IO ()
test p = do
    result <- quickCheckWithResult (stdArgs { maxSuccess = 1000, maxDiscardRatio = 100 }) p
    case result of
        Success _ _ _ -> return ()
        _ -> exitFailure
        
type Triple = (Integer, Integer, Integer)

isTriple :: Triple -> Bool
isTriple (p, q, d) = (q /= 0) && (d > 0) && (not $ isSquare d)
        
reduced' :: Triple -> Bool
reduced' pqd = let (x, y) = toDouble pqd in (x > 1) && (-1 < y) && (y < 0)
        
reduced'' :: Triple -> Bool
reduced'' (p, q, d) = reduced p q (integerSquareRoot d)
        
toDouble :: (Integer, Integer, Integer) -> (Double, Double)
toDouble (p, q, d) =
    let
        pp = fromIntegral p
        qq = fromIntegral q
        dd = sqrt $ fromIntegral d
        x  = (pp + dd) / qq
        y  = (pp - dd) / qq
    in
        (x, y)
        
tripleProp :: (Triple -> Property) -> Triple -> Property
tripleProp f t = counterexample (show $ toDouble t) (f t)
                
prop_reduced :: Triple -> Property
prop_reduced = tripleProp $ \t -> (isTriple t) ==> (reduced'' t === reduced' t)
  
