{-# Language NoMonomorphismRestriction #-}
module Field where
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
field
   *       *       *    (3,6,0) (4,6,0) (5,6,0) (6,6,0)
       *       *    (2,5,0) (3,5,0) (4,5,0) (5,5,0) (6,5,0)
           *    (1,4,0) (2,4,0) (3,4,0) (4,4,0) (5,4,0) (6,4,0)
            (0,3,0) (1,3,0) (2,3,0) (3,3,0) (4,3,0) (5,3,0) (6,3,0)
                (0,2,0) (1,2,0) (2,2,0) (3,2,0) (4,2,0) (5,2,0)    *
                    (0,1,0) (1,1,0) (2,1,0) (3,1,0) (4,1,0)    *       *
                        (0,0,0) (1,0,0) (2,0,0) (3,0,0)    *       *       *

is represented as                when turned counterclockwise:   when turned clockwise:
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


