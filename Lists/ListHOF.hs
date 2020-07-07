module Lists.ListHOF where 

import Prelude hiding (filter, takeWhile, dropWhile, span, break, map, concat, concatMap, and, or, all, any, zipWith)
import Data.Char

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

readDigits :: String -> (String, String)
readDigits x = span isDigit x

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 [] = []
filterDisj p1 p2 (x:xs)
    | (p1 x) || (p2 x) = x : filterDisj p1 p2 xs
    | otherwise        = filterDisj p1 p2 xs 

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = (qsort lesser) ++ [p] ++ (qsort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss  

--  concatMap (++"! ") ["hello", "hi"]  ->  "hello! hi! "
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concat . map (\x -> [x^2, x^3])

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = [y | p <- perms xs, y <- interleave p]
    where
        interleave []     = [[x]]
        interleave (y:ys) = (x:y:ys) : map (y:) (interleave ys)

--

and, or :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

or []     = False
or (x:xs) = x || or xs

-- all odd [1,3,42]  ->  False
all :: (a -> Bool) -> [a] -> Bool   
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool   
any p = or . map p

--

delAllUpper :: String -> String
delAllUpper [] = []
delAllUpper xs = if (length (foo xs)) > 0 then init . foo $ xs else foo $ xs 

foo = concatMap upperWord . words
upperWord w = if all isUpper w then "" else w ++ " " 

-- revWords "hello world"  ->  "olleh dlrow"
revWords :: String -> String
revWords = unwords . map reverse . words

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []     _      = []
zipWith _ _      []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
