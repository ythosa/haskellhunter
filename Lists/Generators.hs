module Lists.Generators where

import Prelude hiding (repeat, replicate, cycle, iterate)

nats n = n : nats (n+1)
firstFiveNats = take 5 $ nats 5 -- -> [5,6,7,8,9]

{-
    head (x:xs) = x
    head []     = error "empty list"

    head $ nats 42 -> return 42
    Why??

    head $ nats 42 -> head (42 : nats(42 + 1)) -> 42 /

    Yay o_o
-}

natSquares = map (^2) $ nats 1
-- take 10 natSquares  ->  [1,4,9,16,25,36,49,64,81,100]

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- repeat :: a -> [a]
-- repeat x = xs where xs = x : xs
repeat = iterate helper 
    where helper = \x -> x

replicate :: Int -> a -> [a]
replicate n x = take n $ repeat x

cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = ys where ys = xs ++ ys

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

--

firstTen = [1..10] -- -> [1,2,3,4,5,6,7,8,9,10]
oddNums = [1,3..10] -- -> [1,3,5,7,9]

someList = [1..20]
listWithComprehension = [x^2 | x <- someList, x^2 < 200]

pairs = [(x, y) | x <- [1,2], y <- [3,4]]

--

{- coins = [2, 3, 7]: 
   GHCi> change 7  ->  [[2,2,3],[2,3,2],[3,2,2],[7]] -}

coins ::(Ord a, Num a) => [a]
coins = [5,9,13]
 
change :: (Ord a, Num a) => a -> [[a]]
change 0      = [[]]
change amount = [ c:cs | c <- coins, amount >= c, cs <- change (amount - c) ]
