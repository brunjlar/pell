module Math.NumberTheory.Pell.PQa
    ( PQa(..)
    , pqa
    , reduced
    , period
    ) where

import Data.Ratio                       ((%))
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)

data PQa = PQa
    { a  :: Integer
    , b  :: Integer
    , g  :: Integer
    , a' :: Integer
    , p  :: Integer
    , q  :: Integer
    } deriving Show

pqa :: Integer -> Integer -> Integer -> [PQa]
pqa p0 q0 d
    | q0 == 0                     = error "Q0 must not be zero."
    | d <= 0                      = error "D must be positive."
    | isSquare d                  = error $ "D must not be a square, but D == " ++ show dd ++ "^2."
    | (p0 * p0 - d) `mod` q0 /= 0 = error $ "P0^2 must be equivalent to D modulo Q0, but "
                                            ++ show p0 ++ "^2 == " ++ show (p0 `mod` q0) ++ " /= " ++ show (d `mod` q0)
                                            ++ " == " ++ show d ++ " (mod " ++ show q0 ++ ")"
    | otherwise                   = go p0 q0 (PQa 0 1 (-p0) undefined undefined undefined) (PQa 1 0 q0 undefined undefined undefined)
    where
        dd = integerSquareRoot d
        go :: Integer -> Integer -> PQa -> PQa -> [PQa]
        go p' q' x y =
            let
                _a' = if q' > 0 then floor $ (p' + dd) % q' else floor $ (p' + dd + 1) % q'
                _a  = _a' * a y + a x
                _b  = _a' * b y + b x
                _g  = _a' * g y + g x
                _p  = _a' * q' - p'
                _q  = (d - _p * _p) `div` q'
                z   = PQa _a _b _g _a' p' q'
            in
                z : go _p _q y z

reduced :: Integer -> Integer -> Integer -> Bool
reduced x y dd
    | y > 0     = (dd >= y - x) && (dd <  x + y) && (dd >= x)
    | otherwise = (dd <  y - x) && (dd >= x + y) && (dd <  x)

period :: Integer -> Integer -> Integer -> (Int, [PQa])
period p0 q0 d = u [] 0 $ pqa p0 q0 d where
    dd = integerSquareRoot d
    u acc i (x : xs)
        | reduced (p x) (q x) dd = v (x : acc) i (p x) (q x) xs
        | otherwise              = u (x : acc) (succ i) xs
    u _ _ _                      = error "algorithm error"
    v acc i pp qq (x : xs)
        | (pp == p x) && (qq == q x) = (i, reverse acc)
        | otherwise                  = v (x : acc) i pp qq xs
    v _ _ _ _ _                      = error "algorithm error"
