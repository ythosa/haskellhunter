module Lists.ListFunctions where

import Prelude hiding (length,(++),null)

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
