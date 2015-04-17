module Math.NumberTheory.Moduli.SquareRoots (sqrts) where

import Data.List (sort)
import Math.NumberTheory.Primes.Factorisation (factorise)
import Math.NumberTheory.Moduli (chineseRemainder, sqrtModPPList)

chineseRemainders :: [([Integer], Integer)] -> [Integer]
chineseRemainders = fst . go where
    go :: [([Integer], Integer)] -> ([Integer], Integer)
    go [] = ([0], 1)
    go xsm = foldr1 f xsm where
        f (xs, m) (ys, n) = (xys, lcm m n) where
            xys = do
                x <- xs
                y <- ys
                case chineseRemainder [(x, m), (y, n)] of
                    Just z  -> return z
                    Nothing -> []

sqrtsPP :: Integer -> (Integer, Int) -> [Integer]
sqrtsPP 1 (2, 1)           = [1]
sqrtsPP 0 (p, e)           = takeWhile (< p ^ e) $ map (* q) [0..] where
                                q = p ^ f
                                f = (if even e then e else succ e) `div` 2
sqrtsPP a (p, e)
    | a `mod`  p      /= 0 = sqrtModPPList a (p, e)
    | a `mod` (p * p) /= 0 = []
    | otherwise            = do
                                x <- sqrtsPP (a `div` (p * p)) (p, e - 2)
                                takeWhile (< m) [p * x + i * p ^ (e - 1) | i <- [0..]]
--  | otherwise    = [x | x <- [0 .. (m - 1)], (x * x - a) `mod` m == 0] 
    where
        m = p ^ e

sqrts :: Integer -> Integer -> [Integer]
sqrts a m
    | a <  0       = error $ "a must not be negative, but a == " ++ show a ++ " < 0."
    | otherwise    = sort $ chineseRemainders $ map f $ factorise m where
                        f (p, e) = (sqrtsPP (a `mod` p ^ e) (p, e), p ^ e)
