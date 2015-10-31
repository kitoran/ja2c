{-# Language NoMonomorphismRestriction #-}
module GameLogicPure where

import Data.List

import Field

merge1 a = prefix ++ (mergeM list) ++ postfix 
    where (prefix, tlist) = span (== (-1)) a
          (list, postfix) = break (== (-1)) tlist
--          mergeM::[Int] -> [Int] -- unfortunately, has nothing to do with monads
          mergeM []          = []
          mergeM [a]         = [a]
          mergeM (0:xs)      = mergeM xs ++ [0]
          mergeM (x:y:xs)
                 | x == 0    = mergeM (y:xs) ++ [0]
                 | y == 0    = mergeM (x:xs) ++ [0]
                 | x /= y    = [x] ++ mergeM (y:xs)
                 | otherwise = [2 * x] ++ mergeM xs ++ [0]

merge :: (Integral a) => a -> Direction -> Field Int -> Field Int
merge a K = rotateCounterclockwise a . map merge1 . rotateClockwise a
merge a I = rotateCounterclockwise a . rotateCounterclockwise a . map merge1 . rotateClockwise a . rotateClockwise a
merge a U = map (reverse . merge1 . reverse)
merge _ H = transpose . map merge1 . transpose
merge a N = rotateClockwise a . map merge1 . rotateCounterclockwise a
merge _ M = map merge1


 