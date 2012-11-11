-----------------------------------------------------------------------------
--
-- Module      :  Problem9
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

module Problem9 (
    problem9
) where

pythagoreans = [(m^2  - n^2, 2*m*n, m^2 + n^2) | m <- [1..], n <- [1..m-1]]
(x, y, z) = head [(x,y,z) | (x, y, z) <- pythagoreans, x+y+z == 1000]



problem9 = x*y*z
