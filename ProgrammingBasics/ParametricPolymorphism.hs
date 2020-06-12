module ParametricPolymorphism where

import Data.Function

id x = x -- parametric polymorphism example

mono :: Char -> Char
mono x = x

-- Without defining the type Haskel defines the most General type
-- In other words, the type of this function will be: t1 -> t2 -> t1
semiMono :: Char -> a -> Char
semiMono x y = x


multSecond = g `on` h
g a b = a * b
h a = snd a


-- Lambda functions
lenVec x y = sqrt $ x^2 + y^2
lenVec' x = \y -> sqrt $ x^2 + y^2
lenVec'' = \x -> \y -> sqrt $ x^2 + y^2
lenVec''' = \x y -> sqrt $ x^2 + y^2


p1 = ((1, 2), (3, 4))
p2 = ((5, 6), (7, 8))

sumFstFst = (+) `on` helper
        where helper pp = fst $ fst pp

sumFstFst' = (+) `on` (\pp -> fst $ fst pp)


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

sum3squares = (\x y z -> x+y+z) `on3` (^2)


-- Compositions
sumFstFst'' = (+) `on` (fst . fst)


bigFunc = f . g . h
        where
                h x = if x > 42 then x else 42
                g x = x ^ 3
                f x = log x


-- Parametric Polymorphism in tuples and lists
tup = (,) True 3

dup x = (x, x)


-- Mapping
avg :: (Double,Double) -> Double
avg (x, y) = (x + y) / 2

--avgandsqr = avg `on` (^2) - Error
avgandsqr = curry avg `on` (^2)

mycurry f a b = f (a, b)

myswap = f (g h)
        where
                h = (,)
                g = flip
                f = uncurry
