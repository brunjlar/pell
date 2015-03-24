module Math.NumberTheory.Pell where

import Data.Ratio ((%))
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)

data PQa = PQa {
    a  :: Integer,
    b  :: Integer,
    g  :: Integer,
    a' :: Integer,
    p  :: Integer,
    q  :: Integer } deriving Show
    
pqa :: Integer -> Integer -> Integer -> [PQa]
pqa p0 q0 d
    | q0 == 0                  = error "Q0 must not be zero."
    | d <= 0                   = error "D must be positive."
    | isSquare d               = error $ "D must not be a square, but " ++ (show d) ++ " = " ++ (show dd) ++ "^2."
    | (p0^2 - d) `mod` q0 /= 0 = error "P0^2 must be equivalent to D modulo Q0."
    | otherwise                = go p0 q0 (PQa 0 1 (-p0) undefined undefined undefined) (PQa 1 0 q0 undefined undefined undefined)
    where
        dd = integerSquareRoot d
        go :: Integer -> Integer -> PQa -> PQa -> [PQa]
        go p q x y =
            let
                _a' = if q > 0 then floor $ (p + dd) % q else floor $ (p + dd + 1) % q
                _a  = _a' * (a y) + (a x)
                _b  = _a' * (b y) + (b x)
                _g  = _a' * (g y) + (g x)
                _p  = _a' * q - p
                _q  = (d - _p^2) `div` q
                z   = (PQa _a _b _g _a' p q)
            in
                z : (go _p _q y z)
        
naive :: Integer -> Integer -> [(Integer, Integer)]
naive d n = [(integerSquareRoot $ n + d * y^2, y) | y <- [1..], isSquare $ n + d * y^2] 
        
solve :: Integer -> Integer -> [(Integer, Integer)]
solve d n
    | (n == (-1)) && (even l) = []
    | (n == (-1)) && (odd l)  = h d x  y  x' y'
    | (n ==   1 ) && (even l) = h d x  y  x  y
    | (n ==   1 ) && (odd l)  = h d x' y' x' y'
    where
        
        (l, x, y) =
            let
                pq  = pqa 0 1 d
                (l, _, x, y) = head $ filter (\(_, qq, _, _) -> qq == 1) $ zipWith3 (\i x y -> (i, q x, g y, b y)) [1..] (tail pq) pq
            in
                (l, x, y)

        (x', y')  = (x^2 + d * y^2, 2 * x * y)                
    
        h :: Integer -> Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)]
        h d x0 y0 t u = let xys = (x0, y0) : [(t * x + u * y * d, t * y + u * x) | (x, y) <- xys] in xys
    
    