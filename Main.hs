main = putStrLn "Hm... Haskell, it will kill me some time ... :c"


-- FUNCTIONS

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


-- OPERATORS --

maxOperatorStyle x y = x `max` y
sumFuncStyle x y = (+) x y

{-
        Usable symbols in custom operators: ! # & % $ * + . / < = > ? @ \ ^ | - ~
-}

infixl 6 ^+^
--a ^+^ b = a ^ 2 + b ^ 2
(^+^) a b = a ^ 2 + b ^ 2
customSumSquares x y = (^+^) x y

a |-| b = abs(a - b)

-- (2 /) 4  or  (/ 4) 2   -   section syntax

-- f $ x = f x
-- sin (pi / 2)    ->    sin $ pi / 2
-- logBase 4 (min 20 (9 + 7) == logBase 4 $ min 20 $ 9 + 7
