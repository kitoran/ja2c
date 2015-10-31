{-# Language NoMonomorphismRestriction #-}
module Interaction where

import Control.Monad

import Data.List
import Data.Char (toUpper)

import System.Random

import Field
import GameLogicPure

prompt = ">"
loop::Field Int -> IO ()
loop field = do prprint field
                putStr (prompt ++ " ")
                s <- getLine                      --may be it's better to use getContents 
                when (s /= "q")
                     (do let d = read $ map toUpper s -- ::Direction
                         let tf = merge 4 d field     ---- !!! 4
                         let zeros = findblanks tf
                         ix <- randomRIO (0, length zeros-1)
                         let sc = zeros !! ix
                         isItFour <- randomIO -- ::Bool
                         loop $ (tf =# sc) (if isItFour then 4 else 2))


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

