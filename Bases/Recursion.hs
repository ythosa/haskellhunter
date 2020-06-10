module Recursion where


factorial :: Integer -> Integer
factorial n = if n == 0 then 1 else n * factorial(n - 1)


factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = if n < 0 then error "Invalid parameter. Must be >= 0" else n * factorial'(n - 1)


factorial'' :: Integer -> Integer
factorial'' 0 = 1
factorial'' n | n < 0 = error "Invalid parameter. Must be >= 0"
              | n > 0 = n * factorial''(n - 1)


factorial''' :: Integer -> Integer
factorial''' n | n == 0    = 1
               | n > 0     = n * factorial'''(n-1)
               | otherwise = error "Invalid parameter. Must be >= 0"


factorial4 :: Integer -> Integer
factorial4 n | n >= 0 = helper 1 n
             | otherwise = error "Invalid parameter. Must be >= 0"

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)


doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial(n-2)


fibonacci :: Integer -> Integer
fibonacci n | n == 0    = 0
            | n == 1    = 1
            | n > 1     = fibonacci (n - 1) + fibonacci (n - 2)
            | n == (-1) = 1
            | n < (-1)  = fibonacci (n + 2) - fibonacci (n + 1)


fibFast n | n >= 0 = goPos n (0, 1)
          | n <= 0 = goNeg n (0, -1)

goPos n (a, b) | n == 0    = a
               | otherwise = goPos (n-1) (b, a + b)

goNeg n (a, b) | n == 0    = (-a)
               | otherwise = goNeg (n+1) (b, a - b)


primes = filterPrime [2..]
        where filterPrime (p : xs) =
                p : filterPrime [x | x <- xs, x `mod` p /= 0]

