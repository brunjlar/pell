module Math.NumberTheory.Pell.IntegerD (
    IntegerD,
    getD,
    root,
    conjugate,
    norm,
    trace,
    toPair,
    toDouble) where

import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)

newtype D = D Integer deriving (Show, Eq)

makeD :: Integer -> D
makeD d
    | d <= 0     = error $ "D must be positive, but is " ++ (show d) ++ "."
    | isSquare d = error $ "D must not be a square, but " ++ (show $ integerSquareRoot d) ++ "^2 == " ++ (show d) ++ "."
    | otherwise  = D d
    
data IntegerD = Pure Integer | Mixed Integer Integer D deriving Eq

getD :: IntegerD -> Maybe Integer
getD (Pure _)          = Nothing
getD (Mixed _ _ (D d)) = Just d

root :: Integer -> IntegerD
root d = Mixed 0 1 (makeD d)

conjugate :: IntegerD -> IntegerD
conjugate (Pure x) = Pure x
conjugate (Mixed x y d) = Mixed x (-y) d

norm :: IntegerD -> Integer
norm x = let (Pure n) = x * (conjugate x) in n

trace :: IntegerD -> Integer
trace x = let (Pure t) = x + (conjugate x) in t

toPair :: IntegerD -> (Integer, Integer)
toPair (Pure x)      = (x, 0)
toPair (Mixed x y _) = (x, y)

instance Show IntegerD where
    show (Pure x)   = show x
    show (Mixed x y (D d))
        | x == 0    = "(" ++ y0 ++ ")"
        | otherwise = "(" ++ (s x) ++ " " ++ y1 ++ ")"
        where
            sd = "root " ++ (show d)
            y0
                | y ==  1   = sd
                | y == -1   = "- " ++ sd
                | otherwise = (s y) ++ " * " ++ sd
            y1
                | y >   0   = "+ " ++ y0
                | otherwise = y0
            s x
                | x > 0 = show x
                | x < 0 = "- " ++ (show (-x))
    
positive :: IntegerD -> Bool
positive (Pure x)          = x > 0
positive (Mixed x y (D d))
    | (x >= 0) && (y > 0)  = True
    | (x >= 0) && (y < 0)  = x * x > d * y * y
    | otherwise            = not $ positive (Mixed (-x) (-y) (D d))
    
instance Num IntegerD where

    (Pure x)      + (Pure x')        = Pure (x + x')
    (Pure x)      + (Mixed x' y' d') = Mixed (x + x') y' d'
    (Mixed x y d) + (Mixed x' y' d')
        | d /= d'                    = error "Can't add elements of different fields."
        | y == (-y')                 = Pure (x + x')
        | otherwise                  = Mixed (x + x') (y + y') d
    a             + b                = b + a

    (Pure x)      * (Pure x')        = Pure (x * x')
    (Pure x)      * (Mixed x' y' d')
        | x == 0                     = Pure 0
        | otherwise                  = Mixed (x * x') (x * y') d'
    (Mixed x y d) * (Mixed x' y' d')
        | d /= d'                    = error "Can't multiply elements of different fields."
        | otherwise                  = let
                                            D dd = d
                                            x''  = x * x' + y  * y' * dd
                                            y''  = x * y' + x' * y
                                       in
                                            if y'' == 0 then Pure x'' else Mixed x'' y'' d
    a             * b                = b * a
    
    negate (Pure x)                  = Pure (-x)
    negate (Mixed x y d)             = Mixed (-x) (-y) d

    fromInteger n = Pure n
    
    signum a
        | a == 0                     = 0
        | positive a                 = 1
        | otherwise                  = -1
        
    abs a                            = a * signum a
    
toDouble :: IntegerD -> Double
toDouble (Pure x)          = fromInteger x
toDouble (Mixed x y (D d)) = (fromInteger x) + (fromInteger y) * (sqrt $ fromInteger d)

instance Ord IntegerD where
    a <= b = (signum $ b - a) /= (-1)
