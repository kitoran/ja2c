{-# Language NoMonomorphismRestriction #-}
module Interaction where

import Data.List

import Field


prprint a = putStr $ prprintiter 0 $ reverse $ transpose a
prprintiter _ [] = "\n"
prprintiter n (x:xs) = take ((n*5) `div` 2) (repeat ' ') 
                    ++ intercalate " " (map myshow x) ++ "\n\n" ++ prprintiter (n+1) xs
    where myshow a --this ugly code should be replaced by printf function
                | a == -1   = "    "
                | a < 10    = "   " ++ show a
                | a < 100   = "  " ++ show a
                | a < 1000  = " " ++ show a
                | otherwise = show a

