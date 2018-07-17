module Homework where
--
import Data.List 
import Data.List.Split
import Data.Time.Clock ( UTCTime(..) )
import Data.Time.Calendar ( Day, gregorianMonthLength, fromGregorian )
import Data.Time.Format ( formatTime, defaultTimeLocale )
--

-- Task 01

data Pred = Val Bool | Not Pred | Or Pred Pred | And Pred Pred

eval :: Pred -> Bool
eval (Val a)     = a
eval (Not p)     = not $ eval p
eval (Or p1 p2)  = (eval p1) || (eval p2)
eval (And p1 p2) = (eval p1) && (eval p2)

-- Task 02

data DayDesc = Desc String String 
  deriving Show

convertTeenthToDay s = (fst $ splitAt ((length s) - 6) s ) ++ "day"

getNthWeekday :: String -> Int -> Int -> Int -> Integer -> Int
getNthWeekday w n d m y 
  | weekday == w && n == 1                = d
  | weekday == w && n > 0                 = getNthWeekday w (n-1) (d+1) m y 
  | otherwise                             = getNthWeekday w n (d+1) m y
  where date = fromGregorian y m d
        weekday = (formatTime defaultTimeLocale "%A" date) 

getFirstWeekday :: String -> Int -> Int -> Integer -> Int
getFirstWeekday w d m y = getNthWeekday w 1 d m y

convertToDayDesc :: String -> Int -> Integer -> DayDesc
convertToDayDesc s m y
  | dayIndex > 5   = Desc "second" niceDay
  | otherwise      = Desc "third" niceDay
  where niceDay = convertTeenthToDay s
        dayIndex = getFirstWeekday niceDay 1 m y

convertMonthToInt :: String -> Int
convertMonthToInt "January"   = 1
convertMonthToInt "February"  = 2
convertMonthToInt "March"     = 3
convertMonthToInt "April"     = 4
convertMonthToInt "May"       = 5
convertMonthToInt "June"      = 6
convertMonthToInt "July"      = 7
convertMonthToInt "August"    = 8
convertMonthToInt "September" = 9
convertMonthToInt "October"   = 10
convertMonthToInt "November"  = 11
convertMonthToInt "December"  = 12

mapOrderedDay :: String -> Int
mapOrderedDay "first"  = 1
mapOrderedDay "second" = 2
mapOrderedDay "third"  = 3
mapOrderedDay "fourth" = 4

convertDescToDay :: DayDesc -> Int -> Integer -> Day
convertDescToDay (Desc d w) m y = fromGregorian y m $ getNthWeekday w (mapOrderedDay d) 1 m y

dateFromDescription :: String -> Day
dateFromDescription s
  | length xs == 6      = convertDescToDay (Desc p1 p2) (convertMonthToInt p4) (read $ xs !! 5)
  | otherwise           = convertDescToDay 
                            (convertToDayDesc p1 (convertMonthToInt p3) (read p4)) 
                            (convertMonthToInt p3) (read p4)

  where xs@(_:p1:p2:p3:p4:_) = splitOn " " s

-- Task 03

data Tree a
  = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

-- a)
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _ Leaf = Leaf
treeFilter p (Node a t1 t2)
  | not $ p a     = Leaf
  | otherwise     = Node a (treeFilter p t1) (treeFilter p t2)

-- b)

levelMap' :: Int -> (Int -> a -> b) -> Tree a -> Tree b
levelMap' n f Leaf           = Leaf
levelMap' n f (Node a t1 t2) = Node (f n a) (levelMap' (n+1) f t1) (levelMap' (n+1) f t2)

levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f t = levelMap' 0 f t 

-- c)
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf _                 = True
isSubtree t1 Leaf                = False
isSubtree t1 t2@(Node a t21 t22) = (t1 == t2) || isSubtree t1 t21 || isSubtree t1 t22

-- Task 04
data Category

parseCategories :: [String] -> [Category]
parseCategories = undefined

printCategories :: [Category] -> [String]
printCategories = undefined
