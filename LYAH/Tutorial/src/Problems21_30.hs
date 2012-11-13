module Problems21_30 where
import Data.List

-- Problem 21: Insert an element at a given position into a list.
-- insertAt 'X' "abcd" 2 = "aXbcd"
insertAt item list index = take (index - 1) list ++ item : drop (index - 1) list

-- Problem 22: Create a list containing all integers within a given range.
-- range 4 9 = [4,5,6,7,8,9]
range min max = [min..max]

data Person = Person {name :: String, firstName :: String, age :: Int}
