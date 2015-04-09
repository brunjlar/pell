module Math.NumberTheory.Pell.Test.IntegerD where

import Math.NumberTheory.Pell.IntegerD
import Math.NumberTheory.Powers.Squares (isSquare)
import Test.QuickCheck

genD :: Gen Integer
genD = suchThat arbitrary $ \d -> (d > 0) && (not $ isSquare d)
       
fromTriple :: Integer -> Integer -> Integer -> IntegerD
fromTriple x y d = (fromInteger x) + (fromInteger y) * (root d)

genIntegerD :: Integer -> Gen IntegerD
genIntegerD d = oneof [
    arbitrary >>= return . fromInteger,
    do
        x <- arbitrary
        y <- arbitrary
        return $ fromTriple x y d]

shrinkIntegerD :: Integer -> IntegerD -> [IntegerD]
shrinkIntegerD d xy = [fromTriple x' y' d | (x', y') <- [(x', y) | x' <- shrink x] ++ [(x, y') | y' <- shrink y]] where
    (x, y) = toPair xy

instance Arbitrary IntegerD where

    arbitrary = genD >>= genIntegerD

    shrink xy = case getD xy of
        Just d  -> shrinkIntegerD d xy
        Nothing -> let (x, _) = toPair xy in map fromInteger $ shrink x

newtype CompatibleDPair = CompatibleDPair (IntegerD, IntegerD) deriving (Show, Eq)

instance Arbitrary CompatibleDPair where

    arbitrary = do
        d <- genD
        a <- genIntegerD d
        b <- genIntegerD d
        return $ CompatibleDPair (a, b)

    shrink (CompatibleDPair (a, b)) = [CompatibleDPair (a', b') | (a', b') <- [(a', b) | a' <- shrink a] ++ [(a, b') | b' <- shrink b]]

prop_norm_multiplicative :: IntegerD -> IntegerD -> Bool
prop_norm_multiplicative x y = norm (x * y) == (norm x) * (norm y)
