module Bases.Operators where

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
