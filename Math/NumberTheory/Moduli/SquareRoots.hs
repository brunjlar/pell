-- |
-- Module:      Math.NumberTheory.Moduli.SquareRoots
-- Copyright:   (c) 2016 by Dr. Lars Brünjes
-- Licence:     MIT
-- Maintainer:  Dr. Lars Brünjes <brunjlar@gmail.com>
-- Stability:   Provisional
-- Portability: portable
--
-- This module provides a function to find all square roots of a number modulo another number.
module Math.NumberTheory.Moduli.SquareRoots
    ( sqrts
    ) where

import Data.List                (sort)
import Math.NumberTheory.Primes (factorise)
import Math.NumberTheory.Moduli (sqrtsModFactorisation)

-- |@sqrts a m@ finds all square roots of @a@ modulo @m@,
--  where @a@ is an arbitrary integer and @m@ is a positive integer.
sqrts :: Integer -> Integer -> [Integer]
sqrts a m = sort (sqrtsModFactorisation a (factorise m))
