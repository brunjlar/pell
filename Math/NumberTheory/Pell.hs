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
    | (    n == (-1)) && (even l1)        = []
    | (    n == (-1))                     = h1 d x1  y1  x1' y1'
    | (    n ==   1 ) && (even l1)        = h1 d x1  y1  x1  y1
    | (    n ==   1 )                     = h1 d x1' y1' x1' y1'
    | (    n == (-4)) && (even l4)        = []
    | (    n == (-4))                     = h4 d x4  y4  x4' y4'
    | (    n ==   4 ) && (even l4)        = h4 d x4  y4  x4  y4
    | (    n ==   4 )                     = h4 d x4' y4' x4' y4'
    | (abs n ==   4 ) && (d `mod` 4 == 0) = map (\(x, y) -> (2 * x,     y)) $ solve (d `div` 4) (n `div` 4)
    | (abs n ==   4 )                     = map (\(x, y) -> (2 * x, 2 * y)) $ solve  d          (n `div` 4)
    where
        
        (l1, x1, y1) = f 0 1 1
        (x1', y1')   = (x1^2 + d * y1^2, 2 * x1 * y1)                
    
        (l4, x4, y4) = f 1 2 2
        (x4', y4')   = ((x4^2 + d * y4^2) `div` 2, x4 * y4)
    
        f :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
        f p0 q0 q' =
            let
                pq  = pqa p0 q0 d
                (l, _, x, y) = head $ filter (\(_, qq, _, _) -> qq == q') $ zipWith3 (\i x y -> (i, q x, g y, b y)) [1..] (tail pq) pq
            in
                (l, x, y)

        h :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)]
        h r d x0 y0 t u = let xys = (x0, y0) : [((t * x + u * y * d) `div` r, (t * y + u * x) `div` r) | (x, y) <- xys] in xys
        
        h1, h4 :: Integer -> Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)]
        h1 = h 1
        h4 = h 2