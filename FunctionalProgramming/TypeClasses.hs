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
