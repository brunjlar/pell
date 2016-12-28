module Math.NumberTheory.Moduli.SquareRoots.Test
    ( prop_sqrtsPP
    , prop_sqrts
    ) where

import Data.Numbers.Primes                  (primes)
import Math.NumberTheory.Moduli.SquareRoots (sqrts)
import Test.QuickCheck

newtype PrimePower = PrimePower (Integer, Int) deriving (Show, Eq)

evalPP :: PrimePower -> Integer
evalPP (PrimePower (p, e)) = p ^ e

instance Arbitrary PrimePower where

    arbitrary = sized $ \size ->
        flip suchThat ((<= (fromIntegral $ size + 2)) . evalPP) $ do
            indexP <- choose (0, size)
            e <- choose (1, 20)
            return $ PrimePower (primes !! indexP, e)

    shrink (PrimePower (p, e)) =
        map PrimePower $  [(p', e) | p' <- takeWhile (< p) primes] ++ [(p, e') | e' <- [1, e - 1]]

newtype ProblemPP = ProblemPP (Integer, PrimePower) deriving (Show, Eq)

instance Arbitrary ProblemPP where

    arbitrary = do
        pp <- arbitrary
        a <- choose (0, evalPP pp - 1)
        return $ ProblemPP (a, pp)

    shrink (ProblemPP (a, pp)) =
        map ProblemPP $ [(a', pp) | a' <- [0, a - 1]] ++ [(a, pp') | pp' <- shrink pp, evalPP pp' > a]

newtype Problem = Problem (Integer, Integer) deriving (Show, Eq)

instance Arbitrary Problem where

    arbitrary = do
        m <- suchThat arbitrary (> 0)
        a <- choose (0, m - 1)
        return $ Problem (a, m)

    shrink (Problem (a, m)) =
        [Problem (a', m) | a' <- shrink a, a' >= 0] ++
        [Problem (a, m') | m' <- shrink m, m' > a]

naive :: Problem -> [Integer]
naive (Problem (a, m)) = [x | x <- [0 .. (m - 1)], (x * x - a) `mod` m == 0]

prop_sqrts :: Problem -> Property
prop_sqrts p@(Problem (a, m)) = sqrts a m === naive p

prop_sqrtsPP :: ProblemPP -> Property
prop_sqrtsPP (ProblemPP (a, pp)) = prop_sqrts (Problem (a, evalPP pp))
