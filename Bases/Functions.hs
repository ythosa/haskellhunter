module Bases.Functions where


sumSquares x y = x^2 + y^2


f x = if x > 0 then (-x) else 0

g x = f x + 3


sign x = if x == 0 then 0 else if x > 0 then 1 else -1


max5 x = max 5 x

max5' = max 5


discount limit proc sum = if sum > limit then sum * (100 - proc) / 100 else sum

standartDiscount = discount 1000 5


translate languageTo languageFrom text = if languageFrom == "Spanish" then "Spanish:" ++ text
        else if languageFrom == "Russian" then "Russian: " ++ text
        else if languageFrom == "English" then "English: " ++ text else text

translateToRussian languageFrom text = translate "Russian" languageFrom text
