-----------------------------------------------------------------------------
--
-- Module      :  Prob
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

module Prob where
import Data.Ratio
import Data.List
import qualified Data.Map as Map

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

aggregatedProb (Prob xs) = Map.toList . Map.fromListWith (+) $ xs

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\ (x, p) -> (f x, p)) xs

flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\ (x, r) -> (x, p*r)) innerxs

instance Monad Prob where
    return x = Prob [(x, 1)]
    m >>= f = flatten $ fmap f m

data Coin = Heads | Tails deriving (Show, Eq)

coin = Prob [(Heads, 1%2), (Tails, 1%2)]
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a, b, c])
