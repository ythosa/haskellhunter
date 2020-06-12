module ProgrammingBasics.Modules (f) where
-- (f) meaning that only this func will be exported from this module

--import Data.Char -- Import module
--import Data.Char (toUpper, toLower) -- Import only this funcs from Data.Char
--import Data.Char hiding (toLower) -- Import all funcs except toLower

-- To use func `union` certain in Data.List and .Union
-- should write Data.List.union and .Set.union
import Data.List
import Data.Set
--import qualified Data.Set -- To set import `func` union from this module as default

--import Data.List as List -- Custom name for Data.List

import ProgrammingBasics.TypeClasses (avg3)

f x = max (avg3 1 2 3) x


