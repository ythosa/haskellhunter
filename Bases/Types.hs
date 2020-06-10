module Types where

import Data.Char

-- :t <something>
-- Char
-- Bool
-- Num: Int & Integer
-- Fractional: Float & Double


x = 3 :: Int
-- Type of (&&) :: Bool -> Bool -> Bool


-- Imports: import...
-- Modules docs: https://hoogle.haskell.org/


testIsDigit x = isDigit x

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (&&) (isDigit x) (isDigit y) then ((digitToInt x) * 10) + digitToInt y else 100
--twoDigits2Int x y = if (&&) (isDigit x) (isDigit y) then ((digitToInt x) * 10) + digitToInt y else 100

-- Tuples
tup = ('x', True, 1) :: (Char, Bool, Int)

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2 -- AB = √(xb - xa)2 + (yb - ya)2

-- Lists
list = [1, 2, 3] :: [Int]
-- "Hi" == ['H', 'i'] :: [Char] :: String

str = 'H' : 'e' : "llo"
str1 = str ++ " Word!"

smartInterpreter = [1,3..10] -- Output: [1,3,5,7,9]
