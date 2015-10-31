{-# Language NoMonomorphismRestriction #-}
module JA2C where
import Data.List 
data SimpleCoords a = SC a a a
{-
These simple coordinates work like this:
They are more closely related to cartesian coordinates then to barycentric despite number of components
               (1,2,0) (2,2,0)
           (0,1,0) (1,1,0) (2,1,0)
       (0,1,1) (0,0,0) (1,0,0) (2,0,0)
           (0,0,1) (1,0,1) (2,0,1)
               (1,0,2) (2,0,2)
                   (2,0,3)

for arbitrary d (a,b,c) and (a+d, b+d, c+d) correspond to the same point 
coordinates are defined uniquely under condition that its components are non-negative and at least one of them is zero
in my game third coordinate is always zero
-}

--Field contains lists of different? length
type Field a = [[a]]
infixl 9   !#
(!#) :: (Integral b) => Field a -> SimpleCoords b -> Maybe a
[]         !# _          = Nothing
[[]]       !# _          = Nothing
((a:_):_)  !# (SC 0 0 0) = Just a
((x:xs):_) !# (SC 0 y 0) = [xs] !# SC 0     (y-1) 0
(_:ys)     !# (SC x y 0) = ys   !# SC (x-1) y     0
a          !# (SC x y z) = a    !# SC (x-z) (y-z) 0
{-
   *       *       *    (3,6,0) (4,6,0) (5,6,0) (6,6,0)
       *       *    (2,5,0) (3,5,0) (4,5,0) (5,5,0) (6,5,0)
           *    (1,4,0) (2,4,0) (3,4,0) (4,4,0) (5,4,0) (6,4,0)
            (0,3,0) (1,3,0) (2,3,0) (3,3,0) (4,3,0) (5,3,0) (6,3,0)
                (0,2,0) (1,2,0) (2,2,0) (3,2,0) (4,2,0) (5,2,0)    *
                    (0,1,0) (1,1,0) (2,1,0) (3,1,0) (4,1,0)    *       *
                        (0,0,0) (1,0,0) (2,0,0) (3,0,0)    *       *       *
represented as                   when turned:                    when turned clockwise:
000 010 020 030  *   *   *       030 140 250 360  *   *   *      300 200 100 000  *   *   *
100 110 120 130 140  *   *       020 130 240 350 460  *   *      410 310 210 110 010  *   *
200 210 220 230 240 250  *       010 120 230 340 450 560  *      520 420 320 220 120 020  *
300 310 320 330 340 350 360      000 110 220 330 440 550 660     630 530 430 330 230 130 030
 *  410 420 430 440 450 460       *  100 210 320 430 540 650      *  640 540 440 340 240 140
 *   *  520 530 540 550 560       *   *  200 310 420 530 640      *   *  650 550 450 350 250
 *   *   *  630 640 650 660       *   *   *  300 410 520 630      *   *   *  660 560 460 360
-}
rotateClockwise ::(Integral a) => a -> Field Int -> Field Int 
rotateClockwise a f = [[maybe (-1) id   $ f !# SC (a-1-j+i) i 0 | j <- [0 .. a*2-2]]| i <- [0 .. a*2-2]]
rotateCounterclockwise ::(Integral a) => a -> Field Int -> Field Int 
rotateCounterclockwise a f = [[maybe (-1) id   $ f !# SC j (a+j-1-i) 0 | j <- [0 .. a*2-2]]| i <- [0 .. a*2-2]]

--i dont know how to make it less magical
data Direction = K | I | U | H | N | M deriving(Show, Eq, Enum, Ord, Read) -- keys around J key in qwerty keyboard

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

{-
merge::(Integral a) => a -> Direction -> Field Int -> Field Int
merge _ K = map reverse (map merge) . (map reverse) . transpose
merge _ M = map merge1
merge _  = 
-}


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





testfield = [["000", "010", "020", "030", " * ", " * ", " * "],
             ["100", "110", "120", "130", "140", " * ", " * "],
             ["200", "210", "220", "230", "240", "250", " * "],
             ["300", "310", "320", "330", "340", "350", "360"],
             [" * ", "410", "420", "430", "440", "450", "460"],
             [" * ", " * ", "520", "530", "540", "550", "560"],
             [" * ", " * ", " * ", "630", "640", "650", "660"]]
             

testfield2 = [[10,11,12,13,-1,-1,-1],
              [14,15,16,17,18,-1,-1],
              [19,20,21,22,23,24,-1],
              [25,26,27,28,29,30,31],
              [-1,32,33,34,35,36,37],
              [-1,-1,38,39,40,41,42],
              [-1,-1,-1,43,44,45,46]]
              
testfield4 = [[ 0, 1, 1, 1,-1,-1,-1],
              [ 0, 0, 2, 0, 2,-1,-1],
              [ 1, 1, 4, 2, 8, 4,-1],
              [ 2, 2, 2, 2, 2, 8, 4],
              [-1, 2, 4, 4, 1, 8, 1],
              [-1,-1, 2, 4, 1, 1, 2],
              [-1,-1,-1, 8, 16,32,64]]