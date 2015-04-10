module Math.NumberTheory.Pell.Test.Utils (
    test,
    (~~) ) where

import System.Exit (exitFailure)
import Test.QuickCheck

test :: Testable p => p -> IO ()
test p = do
    result <- quickCheckWithResult (stdArgs { maxSuccess = 1000, maxDiscardRatio = 100 }) p
    case result of
        Success _ _ _ -> return ()
        _ -> exitFailure

infix 4 ~~

(~~) :: Double -> Double -> Property
x ~~ y = counterexample ("expected " ++ (show x) ++ " ~~ " ++ (show y)) $ abs (x - y) < 1e-4


