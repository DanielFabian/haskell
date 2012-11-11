-----------------------------------------------------------------------------
--
-- Module      :  Sandbox
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Sandbox where

import Data.List

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n*3 + 1)

numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
res1 = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
res2 = map (\(a, b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
sum' = foldl (+) 0
a ==> b = not a || b
elem' y ys = foldl (\acc x -> (x /= y) ==> acc) False ys
sqrtSums = (+1) . length $ takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]
sumOfCubesUnder10k = sum $ takeWhile (<10000) $ map (^3) [1..]
stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
firstTimeAbout1k = find (\(val,y,m,d) -> val > 1000) stock
