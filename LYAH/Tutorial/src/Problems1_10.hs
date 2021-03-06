
module Problems1_10 where
import Data.List

-- Problem 1: Find the last element of a list.
-- myLast [1,2,3,4] = 4
-- myLast ['x','y','z'] = 'z'
myLast = head . reverse

-- Problem 2: Find the last but one element of a list.
-- myButLast [1,2,3,4] = 3
-- myButLast ['a'..'z'] = 'y'
myButLast = head . drop 1 . reverse

-- Problem 3: Find the K'th element of a list. The first element in the list is number 1
-- myElementAt [1,2,3] 2 = 2
-- myElementAt "haskell" 5 = 'e'
myElementAt (x:_) 1 = x
myElementAt (_:xs) index = myElementAt xs (index - 1)

-- Problem 4: Find the number of elements of a list
-- myLength [123, 456, 789] = 3
-- myLength "Hello, world!" = 13
myLength = sum . map (const 1)

-- Problem 5: Reverse a list
-- myReverse "A man, a plan, a canal, panama!" = "!amanap ,lanac a ,nalp a ,nam A"
-- myReverse [1,2,3,4] = [4,3,2,1]
myReverse = foldl (\ acc x -> x:acc) []

-- Problem 6: Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x)
-- isPalindrome [1,2,3] = False
-- isPalindrome "madamimadam" = True
-- isPalindrome [1,2,4,8,16,8,4,2,1] = True
isPalindrome list = list == reverse list

-- Problem 7: Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
-- flatten (Elem 5) = [5]
-- flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) = [1,2,3,4,5]
-- flatten (List []) = []
flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs

-- Problem 8: Eliminate consecutive duplicates of list elements.
-- compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"] = ["a","b","c","a","d","e"]
compress list = map head . group $ list

-- Problem 9: Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
-- pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
--      = ["aaaa","b","cc","aa","d","eeee"]
--pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) =
    let (group, rest) = span (==x) xs
    in (x:group) : pack rest

-- Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the
-- so-called run-length encoding data compression method. Consecutive duplicates of elements are
-- encoded as lists (N E) where N is the number of duplicates of the element E.
-- encode "aaaabccaadeeee" = [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode list = map (\ group -> (length group, head group)) . group $ list
