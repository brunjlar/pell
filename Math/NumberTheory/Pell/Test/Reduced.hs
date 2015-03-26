module Math.NumberTheory.Pell.Test.Reduced ( prop_reduced ) where

import Math.NumberTheory.Pell.PQa (reduced)
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import Test.QuickCheck
        
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
  
