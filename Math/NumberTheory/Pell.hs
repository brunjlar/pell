-- |
-- Module:      Math.NumberTheory.Pell
-- Copyright:   (c) 2016 by Dr. Lars Brünjes
-- Licence:     MIT
-- Maintainer:  Dr. Lars Brünjes <brunjlar@gmail.com>
-- Stability:   Provisional
-- Portability: portable
--
-- This module provides a function to solve generalized Pell Equations,
-- using the "LMM Algorithm" described by John P. Robertson in
-- <http://www.jpr2718.org/pell.pdf>.
-- A /generalized Pell Equation/ is a diophantine equation of the form
-- @x^2 - dy^2 = n@, where @d@ is a positive integer which is not a square
-- and where @n@ is a non-zero integer.
-- We are looking for solutions @(x,y)@, where @x@ and @y@ are non-negative integers.
module Math.NumberTheory.Pell
    ( Solution
    , solve
    ) where

import Control.Arrow                         ((***))
import Data.List                             (sort, nub)
import Data.Ratio                            ((%))
import Data.Set                              (toList)
import Math.NumberTheory.Moduli.SquareRoots  (sqrts)
import Math.NumberTheory.Pell.PQa            (PQa(..), period)
import Math.NumberTheory.Powers.Squares      (isSquare, integerSquareRoot)
import Math.NumberTheory.ArithmeticFunctions (divisors)

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

-- |Represents a solution to a generalized Pell Equation.
-- The first component is the value of x,
-- the second component that of y.
type Solution = (Integer, Integer)

solvePlusOne :: Integer -> Solution
solvePlusOne d = case getRS d 1 0 of
    Just (r, s)
        | r * r - d * s * s == 1 -> (r, s)
        | otherwise              -> (r * r + d * s * s, let rs = r * s in rs + rs)
    Nothing                      -> error "algorithm error"

solveMinusOne :: Integer -> Maybe Solution
solveMinusOne d = case getRS d 1 0 of
    Just (r, s)
        | r * r - d * s * s == (-1) -> Just (r, s)
        | otherwise                 -> Nothing
    Nothing                         -> error "algorithm error"

getRep :: Integer -> Integer -> Integer -> Integer -> Integer -> Solution -> Maybe Solution -> Maybe Solution
getRep _   1  _ _ _ x _ = Just x
getRep _ (-1) _ _ _ _ y = y
getRep d   _  f m z _ y = case getRS d m z of
                            Nothing                      -> Nothing
                            Just (r, s)
                                | r * r - d * s * s == m -> Just (f * r, f * s)
                                | otherwise              -> case y of
                                                                Nothing     -> Nothing
                                                                Just (t, u) -> Just (f * (r * t + s * u * d), f * (r * u + s * t))

mul :: Integer -> Solution -> Solution -> Solution
mul d (x, y) (r, s) = (x * r + y * s * d, x * s + y * r)

getReps :: Integer -> Integer -> (Solution, [Solution])
getReps d n = ((r, s), reps) where
    (r, s)   = solvePlusOne d
    minusOne = solveMinusOne d
    reps = do
        (f, m, zs) <- fmzs d n
        do
            z <- zs
            case getRep d n f m z (r, s) minusOne of
                Just (x, y) -> [if x >= 0 then (x, y) else mul d (r, s) (x, y)]
                Nothing     -> []

getMinimalReps :: Integer -> Integer -> (Solution, [Solution])
getMinimalReps d n = ((r, s), map toMinimal reps) where
    ((r, s), reps) = getReps d n
    toMinimal (x, y) = minimum $ map (abs *** abs) $ filter (\(x', y') -> x' * y' >= 0) [(x, y), mul d (r, s) (x, y), mul d (r, -s) (x, y)]

-- |@solve d n@ calculates all non-negative integer solutions of the generalized Pell Equation
-- x^2 - @d@y^2 = @n@,
-- where @d@ must be a positive integer which is not a square,
-- and @n@ must be a non-zero integer.
solve :: Integer -> Integer -> [Solution]
solve d n
    | d <= 0     = error $ "D must be positive, but D == " ++ show d ++ "."
    | isSquare d = error $ "D must not be a square, but D == " ++ show (integerSquareRoot d) ++ "^2."
    | n == 0     = error "N must not be zero."
    | otherwise  = case getMinimalReps d n of
                    (_, [])       -> []
                    ((r, s), xys) -> go xys where
                        go xys' = normalize xys' ++ go (step xys')
                        normalize = sort . nub
                        step = map (mul d (r, s))
