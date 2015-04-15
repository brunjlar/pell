module Math.NumberTheory.Pell.Test.Reduced ( prop_reduced ) where

import Math.NumberTheory.Pell.PQa (reduced)
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import Test.QuickCheck
        
data Triple = Triple Integer Integer Integer deriving (Show, Eq)

isProperTriple :: Triple -> Bool
isProperTriple (Triple _ q d) = (q /= 0) && (d > 0) && not (isSquare d)

toDouble :: Triple -> (Double, Double)
toDouble (Triple p q d) =
    let
        pp = fromIntegral p
        qq = fromIntegral q
        dd = sqrt $ fromIntegral d
        x  = (pp + dd) / qq
        y  = (pp - dd) / qq
    in
        (x, y)
        
isReduced :: Triple -> Bool
isReduced t = let (x, y) = toDouble t in (x > 1) && (-1 < y) && (y < 0)
        
genTriple :: Gen Triple
genTriple = flip suchThat isProperTriple $ do
        p <- scale (* 2) arbitrary
        q <- scale (* 2) arbitrary
        d <- scale (* 3) arbitrary
        return $ Triple p q d
        
instance Arbitrary Triple where
    arbitrary = oneof $ map (suchThat genTriple) [isReduced, not . isReduced]
    shrink (Triple p q d) = filter isProperTriple $
        [Triple p' q  d  | p' <- shrink p] ++
        [Triple p  q' d  | q' <- shrink q] ++
        [Triple p  q  d' | d' <- shrink d]
        
reduced' :: Triple -> Bool
reduced' (Triple p q d) = reduced p q (integerSquareRoot d)
        
prop_reduced :: Triple -> Property
prop_reduced t@(Triple _ _ d) =
    counterexample (show $ toDouble t)        $
    classify (reduced' t)       "reduced"     $
    classify (not $ reduced' t) "not reduced" $
    classify (d <= 100)         "d <= 100"    $
    classify (d >  100)         "d >  100"    $
    reduced' t === isReduced t
  
