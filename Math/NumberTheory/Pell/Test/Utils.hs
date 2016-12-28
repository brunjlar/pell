module Math.NumberTheory.Pell.Test.Utils
    ( (~~)
    ) where

import Test.QuickCheck

infix 4 ~~

(~~) :: Double -> Double -> Property
x ~~ y = counterexample ("expected " ++ show x ++ " ~~ " ++ show y) $ abs (x - y) < 1e-4
