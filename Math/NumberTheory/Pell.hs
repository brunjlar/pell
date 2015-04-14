module Math.NumberTheory.Pell ( solve ) where

import Control.Arrow (first, (***))
import Data.List (sort, transpose, nub)
import Data.Ratio ((%))
import Data.Set (toList)
import Math.NumberTheory.Moduli (sqrtModFList)
import Math.NumberTheory.Pell.IntegerD (IntegerD, halve, toPair, root, getD)
import Math.NumberTheory.Pell.PQa (PQa(..), pqa, find, period)
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import Math.NumberTheory.Primes.Factorisation (divisors, factorise)
                   
fmzs :: Integer -> Integer -> [(Integer, Integer, [Integer])]
fmzs d n = map (\f -> let m = n `div` (f^2) in (f, m, zs m)) $ filter (\f -> (n `mod` (f^2)) == 0) $ toList $ divisors n where

    zs :: Integer -> [Integer]
    zs   1  = [0]
    zs (-1) = [0]
    zs   m  = nub $ sort $ map norm $ sqrtModFList d $ factorise am where
        am  = abs m
        am2 = floor (am % 2)
        norm a = if a <= am2 then a else a - am
     
getRS d m z = 
    let
        (_, pqas) = period z (abs m) d
        qrs       = zipWith (\x y -> (q x, g y, b y)) (tail pqas) pqas 
        rss       = map (\(_, r, s) -> (r, s)) $ filter (\(q, _, _) -> abs q == 1) qrs
    in
        case rss of
            [] -> Nothing
            (rs : _) -> Just rs

solve_plus_1 d = case getRS d 1 0 of
    Just (r, s)
        | r^2 - d * s^2 == 1 -> (r, s)
        | otherwise          -> (r^2 + d * s^2, 2 * r * s)

solve_minus_one d = case getRS d 1 0 of
    Just (r, s)
        | r^2 - d * s^2 == (-1) -> Just (r, s)
        | otherwise             -> Nothing

getRep d   1  _ _ _ = Just (solve_plus_1 d)
getRep d (-1) _ _ _ = solve_minus_one d
getRep d   n  f m z = case getRS d m z of
                        Nothing                  -> Nothing
                        Just (r, s)
                            | r^2 - d * s^2 == m -> Just (f * r, f * s)
                            | otherwise          -> case solve_minus_one d of
                                                        Nothing     -> Nothing
                                                        Just (t, u) -> Just (f * (r * t + s * u * d), f * (r * u + s * t))

getReps d n = do
    (f, m, zs) <- fmzs d n
    do
        z <- zs
        case getRep d n f m z of
            Just (x, y) -> return (x, y)
            Nothing     -> []

solve d n 
    | d <= 0     = error $ "D must be positive, but D == " ++ show d ++ "."
    | isSquare d = error $ "D must not be a square, but D == " ++ show (integerSquareRoot d) ++ "^2."
    | n == 0     = error "N must not be zero."
    | otherwise  = case getReps d n of 
                    []  -> []
                    xys -> go xys where
                        (r, s) = solve_plus_1 d
                        go xys' = (normalize xys') ++ go (step xys')
                        normalize xys = nub $ sort $ map (\(x, y) -> (abs x, abs y)) xys
                        step = map (\(x, y) -> (x * r + y * s * d, x * s + y * r))
