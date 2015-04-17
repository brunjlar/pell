module Math.NumberTheory.Moduli.SquareRoots (sqrts) where

import Data.List (sort)
import Math.NumberTheory.Primes.Factorisation (factorise)
import Math.NumberTheory.Moduli (chineseRemainder)

chineseRemainders :: [([Integer], Integer)] -> [Integer]
chineseRemainders = fst . go where
    go :: [([Integer], Integer)] -> ([Integer], Integer)
    go [] = ([0], 1)
    go [(xs, m)] = (xs, m)
    go ((xs, m) : xms) = (zs, lcm m m') where
        (ys, m') = go xms
        zs = do
            x <- xs
            y <- ys
            case chineseRemainder [(x, m), (y, m')] of
                Just z -> return z
                Nothing -> []

sqrtsPP :: Integer -> (Integer, Int) -> [Integer]
sqrtsPP a (p, e) = [x | x <- [0 .. (m - 1)], (x * x - a) `mod` m == 0] where
    m = p ^ e

sqrts :: Integer -> Integer -> [Integer]
sqrts a m
    | a <  0       = error $ "a must not be negative, but a == " ++ show a ++ " < 0."
    | otherwise    = sort $ chineseRemainders $ map f $ factorise m where
                        f (p, e) = (sqrtsPP a (p, e), p ^ e)
