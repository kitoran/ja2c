{-# Language NoMonomorphismRestriction #-}
module Interaction where

import Control.Monad

import Data.List
import Data.Char (toUpper)

import Field
import GameLogicPure

prompt = ">"
loop::Field Int -> IO ()
loop field = do prprint field
                putStr (prompt ++ " ")
                s <- getLine                      --may be it's better to use getContents 
                when (s /= "q") 
                     (let d = read $ map toUpper s ::Direction in loop $ merge 4 d field)


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

