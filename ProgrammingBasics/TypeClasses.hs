module TypeClasses where

import Data.Char

-- Special polymorphism

-- Limits are placed on values (type classes in brackets)
-- (>) :: Ord a => a -> a -> Bool
-- (> 7) :: (Num a, Ord a) => a -> Bool

--x = True + False * False  - Error: no instance for (Num Bool) arising from a use of `+'


-- Implementation in the standard library
{-
class Eq a where
        (==), (/=) :: a -> a -> Bool
        x != y = not (x == y) -- Default implementation
        x == y = not (x != y)

instance Eq Bool where
        True  == True  = True
        False == False = True
        _     == _     = False
--        x != y = not (x == y)

instance (Eq a, Eq b) => Eq (a, b) where
        p1 == p2   =   fst p1 == fst p2  &&  snd p1 == snd p2

class (Eq a) => Ord a where
        (<), (<=), (>), (>=) :: a -> a -> Bool
        max, min :: a -> a -> a
        compare :: a -> a -> Ordering
     -- Minimal complete definition: either compare or <=
-}


class Printable a where
        toString :: a -> String

instance Printable Bool where
        toString True = "true"
        toString False = "false"

instance Printable () where
        toString () = "unit type"
  
instance (Printable a, Printable b) => Printable (a, b) where
        toString (a, b) = concat [ "(", toString a, ",", toString b, ")" ]
{-
class (Printable a, Eq a) => MyClass a where
        ...
-}

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool


ip = show a ++ show b ++ show c ++ show d
a = 127
b = 224
c = 120
d = 12


-- Standart classes
{-
class Enum a where
        succ, pred :: a -> a
        toEnum :: Int -> a
        fromEnum :: a -> Int


class Bounded a where
        minBound, maxBound :: a -> Int
-}


class (Enum a, Bounded a, Eq a) => SafeEnum a where
        ssucc :: a -> a
        ssucc a = if a == maxBound then
                        minBound
                  else
                        succ a

        spred :: a -> a
        spred a = if a == minBound then
                        maxBound
                  else
                        pred a

instance SafeEnum Bool
  

{-
class Num a where
        (+), (-), (*) :: a -> a -> a
        negate :: a -> a
        abs :: a -> a
        signum :: a -> a
        fromInteger :: Integer -> a

        x - y = x + negate y
        negate x = 0 - x
-- LAW: abs x * signum x = x


-}


avg3 :: Int -> Int -> Int -> Double
avg3 a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3.0
