module Homework where
--
import Data.List
--

-- Task 01

{-
  Inspect type signatures of functions that you are supposed to implement
  and and from that information try to think of how Robot and Bearing 
  data types should look like.
-}

data Bearing = North | West | South | East deriving Show

data Robot = Robot
  { direction :: Bearing
  , position :: (Integer,Integer)
  } deriving Show


mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot b p = Robot b p

bearing :: Robot -> Bearing
bearing r = direction r

coordinates :: Robot -> (Integer, Integer)
coordinates r = position r

{- It is MANDATORY to implement 'simulate' function in terms of fold -}
simulate :: Robot -> String -> Robot
simulate r s = foldl (\r x -> action x r) r s

action :: Char -> Robot -> Robot  
action 'A' r = r { position = advance (bearing r) (position r) }
action 'R' r = r { direction = turnRight (bearing r) }
action 'L' r = r { direction = turnLeft (bearing r) }

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance North (x,y)  = (x,y+1)
advance West (x,y)   = (x-1,y)
advance South (x,y)  = (x,y-1)
advance East (x,y)   = (x+1,y)


turnLeft :: Bearing -> Bearing
turnLeft North = West 
turnLeft West  = South 
turnLeft South = East
turnLeft East  = North

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West 
turnRight West  = North 

-- Task 02

data TriangleType = Equilateral | Isosceles | Scalene | Degenerate | Illegal deriving Show

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | a + b == c || a + c == b || b + c == a  = Degenerate
  | a == b && a  == c                       = Equilateral
  | a == b || b == c || c == a              = Isosceles
  | a /= b && b /= c && a /= c              = Scalene
  | otherwise                               = Illegal

-- Task 03

{- some convenient test examples -}
-- splitter " > " " > " => ["", ""]
-- splitter " > " "123 > " => ["123", ""]
-- splitter " > " "123 > 456 > 789" => ["123", "456", "789"]

{-
  you don't have to bother with splitting on an empty list e.g.:
  splitter "" "abcde" => ["a", "b", "c", "d", "e"]
-}

{- It is MANDATORY to implement 'splitter' function in terms of fold -}

-- Unfortunately I didn't have time to debug properly.

splitter :: Eq a => [a] -> [a] -> [[a]]
splitter d xs = init $ fst' $ foldr f ( [[]],0, [], length xs) xs
  where f x (result, i, buffer, total)
           | i == ld                   = ( (drop ld buffer : result) , if chk x then 1 else 0 , if chk x then [] else [x], total - 1)
           | x == d !! (ld - i - 1)     = ( result, i+1, x:buffer, total - 1)
           | total == 1                = ( (x:buffer) : result, 0, buffer, total - 1)
           | otherwise                 = ( result, 0, x:buffer, total - 1) 
        ld = length d
        chk x = ( x == d !! (ld-1)  )
        fst' (x,_,_,_) = x

