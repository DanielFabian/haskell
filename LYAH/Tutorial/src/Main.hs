module Main where

import Sandbox

getOptimalPath = optimalPath . map (\[n, s, c] -> Section n s c) . chunkify 3 . map read . words

main = do
    contents <- getContents
    let path = getOptimalPath contents
        pathString = unwords $ map (show . fst) $ path
        pathPrice = sum $ map snd path
    putStrLn $ "The optimal path is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice
