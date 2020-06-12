module Bases.LocalBindings where

roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = (
                (-b - sqrt (b ^ 2 - 4 * a * c) / ( 2 * a )),
                (-b + sqrt (b ^ 2 - 4 * a * c) / ( 2 * a ))
        )


roots' :: Double -> Double -> Double -> (Double, Double)
roots' a b c =
        let d = sqrt (b ^ 2 - 4 * a * c) in
        ((-b - d) / (2 * a), (-b + d) / (2 * a))


roots'' :: Double -> Double -> Double -> (Double, Double)
roots'' a b c =
        let { d = sqrt (b ^ 2 - 4 * a * c); x1 = (-b - d) / (2 * a); x2 = (-b + d) / (2 * a) }
        in (x1, x2)


roots''' :: Double -> Double -> Double -> (Double, Double)
roots''' a b c =
        let
                d = sqrt (b ^ 2 - 4 * a * c)
                x1 = (-b - d) / doubleA
                x2 = (-b + d) / doubleA
                doubleA = 2 * a
        in (x1, x2)


factorial n
        | n >= 0 = let
                count acc 0 = acc
                count acc n = count (acc * n) (n - 1)
        in count 1 n
        | otherwise = error "n must be >= 0"


rootsDiff a b c = let (x1, x2) = roots a b c in abs $ x1 - x2


{- Task: Implement the "seqA" function that finds elements of the following recurrent sequence
        a_0 = 1; a_1 = 2 ; a_2 = 3; a_{k+3} = a_{k+2} + a_{k+1} - 2 * a_{k}. -}
seqA n =
        let
                find n (a, b, c)
                        | n == 0 = a
                        | n == 1 = b
                        | n == 2 = c
                        | otherwise = find (n - 1) (b, c, (b + c - 2 * a))
        in find n (1, 2, 3)


{- A function that finds the sum and number of digits of the decimal record of a given integer -}
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (sumN (abs x), getN (abs x))
        where getN n | n < 10    = 1
                     | otherwise = 1 + getN (n `div` 10)
              sumN n | n < 10    = n
                     | otherwise = n `mod` 10 + sumN (n `div` 10)


{- A function that finds the value of a certain integral of a given function ff at a given
   interval [a,b][a,b] by the trapezoid method. (A uniform grid; 1000 elementary segments) -}
integration f a b = integ f a b 1000 where
	integ f a b 0 = 0
	integ f a b n = h * (f (a) + f (a + h)) / 2 + integ f (a + h) b (n - 1) where h = (b - a) / n
