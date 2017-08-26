{-# Language NoMonomorphismRestriction #-}
module Interaction where

import Control.Monad
import Control.Monad.IO.Class

import Data.List
import Data.Char (toUpper)
import System.Console.Haskeline
import System.IO
import System.Random

import Text.Read

import Field
import GameLogicPure

prompt = ">"
loop::Field Int -> IO ()
loop field = runInputT defaultSettings go
    where go = do 
                liftIO $ prprint field
                ms <- getInputLine (prompt ++ " ")   --may be it's better to use getContents 
                when (ms /= Just "q" && ms /= Nothing)
                     (do let (Just s) = ms
                         let md = readMaybe $ map toUpper s -- ::Direction
                         case md of Nothing -> liftIO $ loop field
                                    Just d -> (do
                                                 let tf = merge 4 d field     ---- !!! 4
                                                 let zeros = findblanks tf
                                                 ix <- liftIO $ randomRIO (0, length zeros-1)
                                                 let sc = zeros !! ix
                                                 isItFour <- liftIO $ randomIO -- ::Bool
                                                 liftIO $ loop $ (tf =# sc) (if isItFour then 4 else 2))) -- there is a memory leak lol

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

