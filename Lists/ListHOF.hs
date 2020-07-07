module Lists.ListHOF where 

import Prelude hiding (filter, takeWhile, dropWhile, span, break)

{- Functions with arguments of the predicates -}

-- filter (<3) [1,2,3,4,5]  ->  [1,2]
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

-- takeWhile (<3) [1,2,3,1,2]  ->  [1,2]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs  
    | otherwise = []

-- dropWhile (<3) [1,2,3,1,2]  ->  [3,1,2]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')  -- xs == x:xs'
    | p x       = dropWhile p xs'
    | otherwise = xs

-- span (<3) [1,2,3,1,2]  ->  ([1,2],[3,1,2])
span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs = (takeWhile p xs, dropWhile p xs)

-- break (<3) [1,2,3,1,2]  ->  ([],[1,2,3,1,2])
break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span $ not . p



