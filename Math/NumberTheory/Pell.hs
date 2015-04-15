module Math.NumberTheory.Pell ( solve ) where

import Data.List (sort, nub)
import Data.Ratio ((%))
import Data.Set (toList)
import Math.NumberTheory.Moduli.SquareRoots (sqrts)
import Math.NumberTheory.Pell.PQa (PQa(..), period)
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import Math.NumberTheory.Primes.Factorisation (divisors)
                   
fmzs :: Integer -> Integer -> [(Integer, Integer, [Integer])]
fmzs d n = map (\f -> let m = n `div` (f * f) in (f, m, zs m)) $ filter (\f -> (n `mod` (f * f)) == 0) $ toList $ divisors n where

    zs :: Integer -> [Integer]
    zs   1  = [0]
    zs (-1) = [0]
    zs   m  = nub $ sort $ map norm $ sqrts d am where
        am  = abs m
        am2 = floor (am % 2)
        norm x = if x <= am2 then x else x - am
     
getRS :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
getRS d m z = 
    let
        (_, pqas) = period z (abs m) d
        qrs       = zipWith (\x y -> (q x, g y, b y)) (tail pqas) pqas 
        rss       = map (\(_, r, s) -> (r, s)) $ filter (\(q', _, _) -> abs q' == 1) qrs
    in
        case rss of
            [] -> Nothing
            (rs : _) -> Just rs

type Solution = (Integer, Integer)

solve_plus_1 :: Integer -> Solution
solve_plus_1 d = case getRS d 1 0 of
    Just (r, s)
        | r * r - d * s * s == 1 -> (r, s)
        | otherwise              -> (r * r + d * s * s, let rs = r * s in rs + rs)
    Nothing                      -> error "algorithm error"

solve_minus_one :: Integer -> Maybe Solution
solve_minus_one d = case getRS d 1 0 of
    Just (r, s)
        | r * r - d * s * s == (-1) -> Just (r, s)
        | otherwise                 -> Nothing
    Nothing                         -> error "algorithm error"

getRep :: Integer -> Integer -> Integer -> Integer -> Integer -> Maybe Solution
getRep d   1  _ _ _ = Just (solve_plus_1 d)
getRep d (-1) _ _ _ = solve_minus_one d
getRep d   _  f m z = case getRS d m z of
                        Nothing                      -> Nothing
                        Just (r, s)
                            | r * r - d * s * s == m -> Just (f * r, f * s)
                            | otherwise              -> case solve_minus_one d of
                                                            Nothing     -> Nothing
                                                            Just (t, u) -> Just (f * (r * t + s * u * d), f * (r * u + s * t))

getReps :: Integer -> Integer -> [Solution]
getReps d n = do
    (f, m, zs) <- fmzs d n
    do
        z <- zs
        case getRep d n f m z of
            Just (x, y) -> return (x, y)
            Nothing     -> []

solve :: Integer -> Integer -> [Solution]
solve d n 
    | d <= 0     = error $ "D must be positive, but D == " ++ show d ++ "."
    | isSquare d = error $ "D must not be a square, but D == " ++ show (integerSquareRoot d) ++ "^2."
    | n == 0     = error "N must not be zero."
    | otherwise  = case getReps d n of 
                    []  -> []
                    xys -> merge $ go xys where
                        (r, s) = solve_plus_1 d
                        go xys' = (normalize xys') ++ go (step xys')
                        normalize = sort . map (\(x, y) -> (abs x, abs y))
                        step = map (\(x, y) -> (x * r + y * s * d, x * s + y * r))
                        merge (x : y : ys)
                            | x < y     = x : (merge (y : ys))
                            | otherwise = merge (x : ys)
                        merge _         = error "algorithm error"
