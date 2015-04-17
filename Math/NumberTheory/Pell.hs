module Math.NumberTheory.Pell ( solve, getFundamentalReps, getMinimalReps, mul, solve_plus_1 ) where

import Control.Arrow (first, (***))
import Data.List (sort, transpose, nub)
import Data.Ratio ((%))
import Data.Set (toList)
import Math.NumberTheory.Moduli.SquareRoots (sqrts)
import Math.NumberTheory.Pell.PQa (PQa(..), pqa, period)
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import Math.NumberTheory.Primes.Factorisation (divisors, factorise)
                   
fmzs :: Integer -> Integer -> [(Integer, Integer, [Integer])]
fmzs d n = map (\f -> let m = n `div` (f^2) in (f, m, zs m)) $ filter (\f -> (n `mod` (f^2)) == 0) $ toList $ divisors n where

    zs :: Integer -> [Integer]
    zs   1  = [0]
    zs (-1) = [0]
    zs   m  = nub $ sort $ map norm $ sqrts d am where
        am  = abs m
        am2 = floor (am % 2)
        norm a = if a <= am2 then a else a - am
     
mul :: Integer -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mul d (x, y) (r, s) = (x * r + y * s * d, x * s + y * r)
     
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

getFundamentalReps d n = ((r, s), fundamentalReps) where
    (r, s) = solve_plus_1 d
    fundamentalReps = do
        (f, m, zs) <- fmzs d n
        do
            z <- zs
            case getRep d n f m z of
                Just (x, y) -> [if x >= 0 then (x, y) else mul d (r, s) (x, y)]
                Nothing     -> []

getMinimalReps d n = ((r, s), map toMinimal $ fundamentalReps) where
    ((r, s), fundamentalReps) = getFundamentalReps d n
    toMinimal (x, y) = head $ sort $ map (abs *** abs) $ filter (\(x', y') -> x' * y' >= 0) [(x, y), mul d (r, s) (x, y), mul d (r, (-s)) (x, y)]

solve d n 
    | d <= 0     = error $ "D must be positive, but D == " ++ show d ++ "."
    | isSquare d = error $ "D must not be a square, but D == " ++ show (integerSquareRoot d) ++ "^2."
    | n == 0     = error "N must not be zero."
    | otherwise  = case getMinimalReps d n of 
                    (_, [])       -> []
                    ((r, s), xys) -> go xys where
                        go xys' = (normalize xys') ++ go (step xys')
                        normalize = sort . nub . map (abs *** abs) 
                        step = map (mul d (r, s))

equivalent :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer) -> Bool
equivalent d n (x, y) (r, s) = (nDivides $ x * r - d * y * s) && (nDivides $ x * s - y * r) where
    nDivides z = (z `mod` n) == 0
