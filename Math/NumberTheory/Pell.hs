module Math.NumberTheory.Pell ( solve ) where

import Data.List (sort, transpose)
import Data.Ratio ((%))
import Data.Set (toList)
import Math.NumberTheory.Moduli (sqrtModFList)
import Math.NumberTheory.Pell.PQa (PQa(..), pqa)
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import Math.NumberTheory.Primes.Factorisation (divisors, factorise)
                   
naive :: Integer -> Integer -> [(Integer, Integer)]
naive d n = [(integerSquareRoot $ n + d * y^2, y) | y <- [1..], isSquare $ n + d * y^2] 
     
fmzs :: Integer -> Integer -> [(Integer, Integer, [Integer])]
fmzs d n = map (\f -> let m = n `div` (f^2) in (f, m, zs m)) $ filter (\f -> (n `mod` (f^2)) == 0) $ toList $ divisors n where
    zs m = sort $ map norm $ sqrtModFList d $ factorise am where
        am  = abs m
        am2 = floor (am % 2)
        norm a = if a <= am2 then a else a - am
     
equivalent :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer) -> Bool
equivalent d n (x, y) (r, s) = (f $ x * r - d * y * s) && (f $ x * s - y * r) where
    f x = (x `mod` n) == 0
    
solve :: Integer -> Integer -> [(Integer, Integer)]
solve d n
    | d <= 0                              = error "D must be positive."
    | isSquare d                          = error $ "D must not be a square, but D = " ++ (show $ integerSquareRoot d) ++ "^2."
    | n == 0                              = error "N must not be zero."
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
    | (1 < n * n) && (n * n < d)          = let xys = (1, 0) : solve d 1 in interleave $ map (\(r, s) -> expand d r s xys) $ ltD d n
    | otherwise                           = undefined
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
        
        ltD :: Integer -> Integer -> [(Integer, Integer)]     
        ltD d n = map (\(gg, bb, _, f2) -> let f = integerSquareRoot f2 in (f * gg, f * bb)) gbf2 where
            pq  = pqa 0 1 d
            lxy = zipWith3 (\l x y -> (l, q x, g y, b y)) [1..] (tail pq) pq
            gbs = map (\(_, _, gg, bb) -> (gg, bb, gg^2 - d * bb^2)) $ takeWhile (\(l, qq, gg, bb) -> ((qq /= 1) || (odd l))) lxy
            gbf2 = filter (\(_, _, gb, f2) -> (gb * f2 == n) && (isSquare f2)) $ map (\(gg, bb, gb) -> (gg, bb, gb, n `div` gb)) gbs
        
        interleave :: [[a]] -> [a]
        interleave = concat . transpose
        
        expand :: Integer -> Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
        expand d r s = map (\(t, u) -> (r * t + s * u * d, r * u + s * t))