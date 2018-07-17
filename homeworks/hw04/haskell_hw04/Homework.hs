module Homework where
--
import Data.List
import Data.Char
import Data.Function ( fix )
--

-- Task 01

-- non accumulator style
factorial :: (Num a, Eq a) => a -> a
factorial = fix (\rec x -> if x == 0 then 1 else x * rec(x-1)) 

-- non accumulator style
sum' :: Num a => [a] -> a
sum' = fix (\rec (x:xs) -> if null xs then x else x + rec(xs))

-- accumulator style
factorial' :: (Num a, Eq a) => a -> a
factorial' = fix (\rec n x -> if x == 0 then n else rec (n*x) $ (x - 1) ) 1

-- accumulator style
sum'' :: Num a => [a] -> a
sum'' = fix (\rec n (x:xs) -> if null xs then n + x else rec (n+x) $ xs ) 0 

nats :: [Integer]
nats = fix (\rec x -> x : rec(x+1) ) 1 

map' :: (a -> b) -> [a] -> [b]
map' = fix (\rec f (x:xs) -> if null xs then [f x] else f x : rec f xs)

zip' :: [a] -> [b] -> [(a, b)]
zip' = fix (\rec (x:xs) (y:ys) -> if null xs || null ys then [(x,y)] else (x,y) : rec xs ys) 

-- Task 02

subset :: Int -> [a] -> [[a]]
subset 0 _ = [[]] 
subset _ [] = []
subset n (x:xs) = map (x:) (subset (n-1) xs) ++ subset n xs

subsets :: Eq a => Int -> [a] -> [[a]]
subsets n xs = subset n $ nub xs

-- recursively creates 'sub-partitions' by joining single element with list of subsets 
-- e.g. combine 1 [ [2,3] ] => [ [[1,2,3]], [[2,3],[1]] ]
--      combine 1 [ [2],[3] ] => [ [[1,2],[3]], [[2],[1,3]], [[2],[3],[1]] ]
combine :: a -> [[a]] -> [[[a]]]
combine x       [] = [[[x]]]
combine x (xs:xss) = ((x:xs):xss) : map (xs:) (combine x xss)

partitions :: [a] -> [[[a]]]
partitions    [] = [[]]
partitions (x:xs) = concatMap ( x `combine` ) $ partitions xs 

-- Task 03
permutations' :: [a] -> [[a]]
permutations' = undefined
