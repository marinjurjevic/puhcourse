{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
--

{-
    Here you should provide your solutions to in-class exercises.
    
    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.
    
    You should include solutions from following lectures :
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 04-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs

-- EXERCISE 01 =======================================================================

-- Define 'headHunter xss' that takes the head of the first list element. If 
-- the first element has no head, it takes the head of the second element.
-- If the second element has no head, it takes the head of the third element.
-- If none of this works, the function returns an error.
ex411 = headHunter
headHunter :: [[a]] -> a
headHunter ((x:_):_) = x
headHunter (_:(x:_):_) = x
headHunter (_:_:(x:_):_) = x

-- Define 'firstColumn m' that returns the first column of a matrix.
-- firstColumn [[1,2],[3,4]] => [1,3]
-- Check what happens if the input is not a valid matrix.
ex412 = firstColumn
firstColumn :: [[a]] -> [a]
firstColumn m = [ h | (h:_) <- m ] 

-- Define 'shoutOutLoud' that repeats three times the initial letter of each
-- word in a string.
-- shoutOutLoud :: String -> String
-- shoutOutLoud "Is anybody here?" => "IIIs aaanybody hhhere?"
ex413 = shoutOutLoud
shoutOutLoud s = unwords [ h:h:h:r  | (h:r) <- words s ]

-- EXERCISE 02 =======================================================================

-- Define 'pad' that pads the shorter of two the strings with trailing spaces 
-- and returns both strings capitalized.
-- pad :: String -> String -> (String, String)
-- pad "elephant" "cat" => ("Elephant", "Cat     ")

median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: empyt list"
median xs
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l = length xs
        h = l `div` 2
        ys = sort xs


ex421 = pad
pad :: String -> String -> (String,String)
pad x y 
  | l1 >= l2 = ( con x 0, con y $ l1 - l2)
  | otherwise = ( con x $ l2 - l1, con y 0 )
  where l1 = length x
        l2 = length y
        con (c:r)  n = toUpper c : r ++ (take n $ repeat ' ') 

-- Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
-- The quartiles are elements at the first, second, and third quarter of a list
-- sorted in ascending order. (You can use the built-int 'splitAt' function and
-- the previously defined 'median' function.)
-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)
ex422 = quartiles
quartiles :: [Int] -> (Double, Double, Double)
quartiles [] = error "quartiles: empty list"
quartiles xs 
  | odd l  = (median ys1, median ys, median $ tail ys2)
  | otherwise = ( median ys1, median ys, median ys2) 
  where ys = sort xs
        l = length xs
        h = l`div` 2
        (ys1,ys2) = splitAt h ys

-- EXERCISE 03 =======================================================================

-- Redo Exercise 2 using 'let' instead of 'where'.
ex431 = pad'
pad' :: String -> String -> (String,String)
pad' x y = 
  let l1 = length x
      l2 = length y
      con (c:r) n = toUpper c : r ++ (take n $ repeat ' ')
  in
    if l1 >= l2 then
      ( con x 0, con y $ l1 - l2)
    else
      ( con x $ l2 - l1, con y 0 )

ex432 = quartiles'
quartiles' :: [Int] -> (Double, Double, Double)
quartiles' [] = error "quartiles: empty list"
quartiles' xs =
  let ys = sort xs
      l = length xs
      h = l `div` 2
      (ys1,ys2) = splitAt h ys
  in
    if odd l then
      (median ys1, median ys, median $ tail ys2)
    else
      ( median ys1, median ys, median ys2) 

-- EXERCISE 04 =======================================================================

-- Write a function that takes in a pair (a,b) and a list [c] and returns the
-- following string:
-- "The pair [contains two ones|contains one one|does not contain a single one]
-- and the second element of the list is <x>"
ex441 = moo
moo :: Show c => (Int,Int) -> [c] -> String
moo p (_:z:_)  = 
  "The pair " ++
  (case p of 
    (1,1) -> "contains two ones"
    (1,_) -> "contains one one"
    (_,1) -> "contains one one"
    (_,_) -> "does not contain a single one") ++
  " and the second element of the list is " ++ show z

{-LECTURE 05-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

-- EXERCISE 01 =======================================================================

-- Define a recursive function to compute the product of a list of elements.
-- product' :: Num a => [a] -> a
ex511 = product'
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- Define a recursive function 'headsOf' that takes a list of lists and
-- returns a list of their heads.
-- headsOf :: [[a]] -> [a]
-- headsOf [[1,2,3],[4,5],[6]] => [1,4,6]
ex512 = headsOf

headsOf :: [[a]] -> [a]
headsOf  []   = []
headsOf ([]:xss)  = headsOf xss
headsOf (xs:xss) = [head xs] ++ headsOf xss
-- EXERCISE 02 =======================================================================

-- Define a recursive function 'modMult n m xs' that multiplies each element of
-- a list 'xs' with 'n' modulo 'm'.
ex521 = modMult

modMult :: Integral a => a -> a -> [a] -> [a]
modMult _ _ [] = []
modMult n m (x:xs) = x * (n `mod` m) : modMult n m  xs

-- Define a function 'addPredecessor' that adds to each element of a list the
-- value of the preceding element. The first element gets no value added.
-- addPredecessor :: Num a => [a] -> [a]
-- addPredecessor [3,2,1] => [3,5,3]
ex522 = addPredecessor

addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = add 0 xs
  where add _ [] = []
        add n (x:xs) = x + n : add x xs

-- EXERCISE 03 =======================================================================

-- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
-- triplets for which x==y==z.
-- equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]
ex531 = equalTriplets

equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)] 
equalTriplets [] = []
equalTriplets ((a,b,c):xs)
   | a == b && b == c    = (a,b,c) : equalTriplets xs  
   | otherwise           = equalTriplets xs

-- Define your own version of the replicate function:
-- replicate' :: Int -> a -> [a]
ex532 = replicate'
replicate' :: Int -> a -> [a]
replicate' n x 
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

-- EXERCISE 04 =======================================================================

-- Define your own recursive version of the drop function:
-- drop' :: Int -> [a] -> [a].
-- Define drop'' (a wrapper function) so that for n < 0 the function drops
-- the elements from the end of the list. You can use 'reverse'.
ex541 = drop'
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop (n-1) xs

ex541' = drop''
drop'' :: Int -> [a] -> [a]
drop'' n xs
 | n < 0 = reverse $ drop' (-n) (reverse xs)
 | otherwise = drop' n xs

-- Define a recursive function 'takeFromTo n1 n2 xs'.
-- takeFromTo :: Int -> Int -> [a] -> [a]
ex542 = takeFromTo

takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo n1 n2 xs = drop'' n1 $ drop'' (-((length xs)-n2) ) xs

-- EXERCISE 05 =======================================================================

-- Define a recursive function 'eachThird' that retains every third element
-- in a list.
-- eachThird :: [a] -> [a]
-- eachThird "zagreb" => "gb"
ex551 = eachThird

eachThird :: [a] -> [a]
eachThird (x1:x2:x3:xs) = x3 : eachThird xs
eachThird _ = []
-- Define a recursive function 'crossZip' that zips two lists in a "crossing"
-- manner:
-- crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]
ex552 = crossZip

crossZip :: [a] -> [a] -> [(a,a)]
crossZip (a1:a2:as) (b1:b2:bs) = (a1,b2) : (a2,b1) : crossZip as bs
crossZip _ _ = []


-- EXERCISE 06 =======================================================================

-- Write an accumulator-style recursive definition of
-- length' :: [a] -> Int

ex561 = length'
length' :: [a] -> Int
length' xs = len xs 0
  where len []  n = n
        len (x:xs) n = len xs (n+1)

-- Write an accumulator-style recursive definition of
--     maxUnzip :: [(Int, Int)] -> (Int, Int)
-- that returns the maximum element at the first position and the maximum
-- element at the second position in a pair, i.e., it's equivalent to:
--     maxUnzip zs = (maximum xs, maximum ys)
--         where (xs,ys) = unzip zs
-- If the list is empty, return an "empty list" error.
-- Now write a standard recursive definition maxUnzip' (without an accumulator).
ex562 = maxUnzip

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip  [] = error "Empty list"
maxUnzip xs = maxUnzip' xs 0 0
  where maxUnzip' [] n1  n2  = (n1,n2)
        maxUnzip' ((x,y):xs) n1 n2 = maxUnzip' xs (max x n1) (max y n2)

ex562' = maxUnzip'
maxUnzip' :: [(Int,Int)] -> (Int,Int)
maxUnzip' [] = error "Empty list"
maxUnzip' ( (x,y) : []) = (x,y)
maxUnzip' ( (x,y) : zs)  = ( max x $ fst mx, max y $ snd mx) 
  where mx = maxUnzip' zs
        
