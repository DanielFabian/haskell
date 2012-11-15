-----------------------------------------------------------------------------
--
-- Module      :  Chess
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

module Chess where
import Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (col, row) = do
    pm1 <- [(+), (-)]
    pm2 <- [(+), (-)]
    (colDiff,rowDiff) <- [(1, 2), (2, 1)]
    let newCol = col `pm1` colDiff
        newRow = row `pm2` rowDiff
    guard $ newCol `elem` [1..8]
    guard $ newRow `elem` [1..8]
    return (newCol, newRow)

in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 start end = end `elem` in3 start


