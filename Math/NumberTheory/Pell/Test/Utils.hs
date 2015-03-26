module Math.NumberTheory.Pell.Test.Utils ( test ) where

import System.Exit (exitFailure)
import Test.QuickCheck

test :: Testable p => p -> IO ()
test p = do
    result <- quickCheckWithResult (stdArgs { maxSuccess = 1000, maxDiscardRatio = 100 }) p
    case result of
        Success _ _ _ -> return ()
        _ -> exitFailure