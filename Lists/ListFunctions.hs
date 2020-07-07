module Lists.ListFunctions where

import Prelude hiding (length, (++), null, last, init, zip, unzip, take, drop, splitAt, (!!))
import Data.List hiding (length, (++), null, last, init, zip, unzip, take, drop, splitAt, (!!))

list = 5 : [1, 2, 3] -- -> [5,1,2,3]
list1 = list ++ [4, 5] -- -> [5,1,2,3,4,5]

-- pushHead42 [1,2,3] -> [42,1,2,3]
pushHead42 = (42 :)

-- nTimesPushing 1 2 -> [1,1]
nTimesPushing k n = pushing [] k n
        where pushing arr k n | n > 0 = pushing (k : arr) k (n - 1)
                              | n == 0 = arr

fstElement = head [1,2,3] -- -> 1
otherElements = tail [1,2,3] -- -> [2,3]

sndElement = head . tail
sndElement' (_ : xs) = head xs
sndElement'' (_ : x : _) = x
--sndElement [1,2,3] -> 2

head' ((:) x xs) = x -- returns head of the list
tail' (_ : xs) = xs -- returns tail of the list
--tail' (x : xs) = xs

sndHead :: [(a, c)] -> c
sndHead = snd . head
sndHead' ((_, a) : _) = a

length :: [a] -> Int
length []       = 0
length (_ : xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : xs ++ ys

null :: [a] -> Bool
null [] = True
null _  = False


--oddsOnly [2,5,7,10,11,12] -> [5,7,11]
oddsOnly :: Integral a => [a] -> [a]
oddsOnly list = reverseList $ odding list []
    where
        odding [] odded = odded
        odding (x : xs) odded = if x `mod` 2 == 1
            then
                odding xs (x : odded)
            else
                odding xs odded

        reverseList = go []
            where
                go acc [] = acc
                go acc (x:xs) = go (x:acc) xs

last :: [a] -> a
last (x : []) = x
last (x : xs) = last xs

init :: [a] -> [a]
init []       = error "the slice must not be empty"
init [_]      = []
init (x : xs) = x : init xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []        = True
isPalindrome [_]       = True
isPalindrome (a : arr) = a == last arr && isPalindrome(init arr)

zip :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip _      []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs

unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x,y) : xys) =
    let (xs, ys) = unzip xys
    in  (x:xs, y:ys)

-- sum3 [1,2,3] [4,5] [6]  ->  [11,7,3]
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 a b c = calculate $ transpose [a,b,c] where
  calculate as = reverse $ calc [] as where
    calc a [] = a
    calc a (x:xs) = calc (sum x:a) xs

-- groupElems [1,2,2,2,4]  ->  [[1],[2,2,2],[4]]
groupElems :: Eq a => [a] -> [[a]]
groupElems []     = []
groupElems (x:xs) = accum xs [x] []
  where
    accum []     acc     all  = reverse $ acc:all
    accum (x:xs) (z:acc) all | x == z    = accum xs (z:z:acc) all
    accum (x:xs) (z:acc) all | otherwise = accum xs [x] ((z:acc):all)

-- take 1 "Hello"  ->  "H"
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x : take (n-1) xs

-- drop 1 "Hello"  ->  "ello"
drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ []          = []
drop n (x:xs)      = drop (n-1) xs

-- splitAt 2 [1,2,3]  ->  ([1, 2],[3])
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- "Hello" !! 4  ->  'o'
(!!) :: [a] -> Int -> a
(!!) xs     n | n < 0 = error "negative index" 
(!!) []     _         = error "index too large"
(!!) (x:_)  0         = x
(!!) (_:xs) n         = xs !! (n-1)
