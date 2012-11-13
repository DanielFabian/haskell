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


solveRPN = head . foldl calculateUPN [] . words

calculateUPN (y:x:rest) "+" = x + y : rest
calculateUPN (y:x:rest) "*" = x * y : rest
calculateUPN (y:x:rest) "/" = x / y : rest
calculateUPN (y:x:rest) "-" = x - y : rest
calculateUPN (y:x:rest) "^" = x ** y : rest
calculateUPN (x:rest) "ln" = log x : rest
calculateUPN (x:rest) "neg" = -x : rest
calculateUPN list "sum" = [sum list]
calculateUPN list number = read number : list

data Section = Section { north :: Int, south :: Int, crossover :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = North | South | CrossOver deriving (Show)
type Path = [(Label, Int)]

optimalPath = reverse . pickBestPath . foldl roadStep ([], [], 0, 0)

roadStep (northernPath, southernPath, northernPrice, southernPrice) (Section north south crossover) =
    let northToNorth  = northernPrice + north
        northToSouth  = northernPrice + crossover
        southToNorth  = southernPrice + crossover
        southToSouth  = southernPrice + south
        optimalPathToNorth = if northToNorth <= southToNorth
                             then (North, north):northernPath
                             else (CrossOver, crossover):(South, south):southernPath
        optimalPathToSouth = if southToSouth <= northToSouth
                             then (South, south):southernPath
                             else (CrossOver, crossover):(North, north):northernPath
    in (optimalPathToNorth
        ,optimalPathToSouth
        ,min northToNorth southToNorth
        ,min southToSouth southToNorth)
pickBestPath (northernPath, southernPath, northernPrice, southernPrice) =
    if northernPrice <= southernPrice then northernPath else southernPath

chunkify 0 _ = undefined
chunkify _ [] = []
chunkify n list = take n list : (chunkify n $ drop n list)
















