module Problems11_20 where
import Problems1_10
import Data.List

-- Problem 11: Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is
-- simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
-- encodeModified "aaaabccaadeeee"
--      = [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
data RunLength a = Single a | Multiple Int a
    deriving Show

encodeModified list = [toRunLength x | x <- encode list]
    where
        toRunLength (1, x)  = Single x
        toRunLength (many, x) = Multiple many x


-- Problem 12: Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed
-- version.
-- decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
--      = "aaaabccaadeeee"
decodeModified = concat . map decode
    where
        decode (Single x) = [x]
        decode (Multiple many x) = replicate many x

-- Problem 13: Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. I.e. don't
-- explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
-- encodeDirect "aaaabccaadeeee"
--      = [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect list = foldr accumulate [] list
    where
        accumulate x [] = [Single x]
        accumulate new encoded@(Single old : xs)
            | new == old   = Multiple 2 old : xs
            | otherwise = Single new : encoded
        accumulate new encoded@(Multiple count old : xs)
            | new == old = Multiple (count + 1) old : xs
            | otherwise = Single new : encoded

-- Problem 14: Duplicate the elements of a list.
-- dupli [1, 2, 3] = [1,1,2,2,3,3]
dupli = concatMap $ replicate 2

-- Problem 15: Replicate the elements of a list a given number of times.
-- repli "abc" 3 = "aaabbbccc"
repli list count = concatMap (replicate count) list

-- Problem 16: Drop every N'th element from a list.
-- dropEvery "abcdefghik" 3 = "abdeghk"
dropEvery list nth = map fst $ filter ((/= nth) . snd) . zip list $ cycle [1..nth]

-- Problem 17: Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
-- split "abcdefghik" 3 = ("abc", "defghik")
split [] _   = ([], [])
split list 0 = ([], list)
split list@(x:xs) count
    | count < 0 = (list, [])
    | otherwise = let (left, right) = split xs (count-1)
                  in (x:left, right)

-- Problem 18: Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th
-- and k'th element of the original list (both limits included). Start counting the elements with 1.
-- slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 = "cdefg"
slice list i k = take (k - i + 1) . drop (i - 1) $ list

-- Problem 19: Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
-- rotate ['a','b','c','d','e','f','g','h'] 3 = "defghabc"
-- rotate ['a','b','c','d','e','f','g','h'] (-2) = "ghabcdef"
rotate list count = let len = length list in take len . drop (count `mod` len) $ cycle list

-- Problem 20: Remove the K'th element from a list.
-- removeAt 2 "abcd" = ('b',"acd")
removeAt count list =
    let (left, right) = splitAt count list
        (removed:init) = reverse left
    in (removed, reverse init ++ right)
