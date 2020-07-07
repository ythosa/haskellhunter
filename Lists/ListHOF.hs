module Lists.ListHOF where 

import Prelude hiding (filter, takeWhile, dropWhile, span, break)

-- filter (<3) [1,2,3,4,5]  ->  [1,2]
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs


