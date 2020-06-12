module ProgrammingBasics.NonStrictSemantics where

sumIt :: Int -> Int -> Int
sumIt x y = x + y

{- Lazy calculation strategy - Haskell
sumIt (2 + 3) 4
        ~> (2 + 3) 4
        ~> 5 4
        ~> 9
-}

{- Energetic strategy of the calculations
sumIt (2 + 3) 4
        ~> sumIt 5 4
        ~> 5 + 4
        ~> 9
-}


add7 :: Int -> Int -> Int
add7 x y = x + 7
{-
add7 1 (2+3)
        ~> 1 + 7
        ~> 8

add7 1 (2+3)
        ~> add7 1 5
        ~> 1 + 7
        ~> 8
-}


dup :: Int -> (Int, Int)
dup x = (x, x)
{-
dup (2+3)
        ~> (2+3, 2+3)
        ~> (5, 2+3)
        ~> (5,5)

dup (2+3)
        ~> dup 5
        ~> (5, 5)
-}

--The mechanism of separation
{-
dup (2+3)
        ~> (p, p) p = 2+3
        ~> (5,p) p = 5
        ~> (5, 5
-}

foo a = a

bar = const foo


-- Normal forms of expressions
{-
32
(3,4)
/x -> x + 2
-}

-- Not Normal forms of expressions
{-
"Hello" ++ " World"
sin $ pi / 2
(\x -> x + 2) 5
(3, 1+5)
-}

-- Weak header normal forms
{-
\x -> x + 2*3
(3, 1+5)
(,) (4*5)
(+) (7^2)
-}


-- Lazy semantic destroyer
{-
  seq :: a -> b -> b
  seq _|_ b = _|_
  seq a b = b
-}

{-
  ($!) :: (a -> b) -> a -> b
  f $! x = x `seq` f x
-}

--const 42 $ undefined  -- ~> 42
--const 42 $! undefined -- ~> error

factorial :: Integer -> Integer
factorial n | n >= 0    = helper 1 n
            | otherwise = error "arg must be >= 0"
          where
                  helper acc 0 = acc
                  helper acc n = (helper $! (acc * n)) (n - 1)
