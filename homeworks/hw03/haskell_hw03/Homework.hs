module Homework where
--
import Data.List
import Data.Char
import Data.Bits ( xor )
--

-- Task 01

localMaxima :: [Int] -> [Int]
localMaxima xs@(x:y:[]) = [] 
localMaxima (x:ys@(y:z:_)) 
  | y>x && y>z = y : localMaxima ys  
  | otherwise  = localMaxima ys

-- Accumulator version
localMaxima' :: [Int] -> [Int]
localMaxima' xs = maxima xs []
  where maxima (x:y:[])       lm  = lm 
        maxima (x:ys@(y:z:_)) lm 
          | y>x && y>z = maxima ys (y:lm)
          | otherwise  = maxima ys lm

-- Task 02
transform :: [(Int, String)] -> [(Char, Int)]
transform []  = []
transform ((x,[])        : zs) = transform zs
transform ((x,(c:cs))    : zs) = (toLower c,x) : transform ((x,cs):zs)

-- Task 03
rule90 :: [Bool] -> [[Bool]]
rule90 xs = xs : rule90 ys 
  where ys = rule90Step $ False:xs

rule90Step :: [Bool] -> [Bool]
rule90Step (x:y:[]) = [xor x False]
rule90Step (x:xs@(y:z:_)) = (xor x z) : rule90Step xs 

pretty :: [[Bool]] -> String
pretty [] = []
pretty (xs:xss) = [ if x then '#' else ' ' | x <- xs ] ++ "\n" ++  pretty xss

-- Task 04

-- lasStep makes one iteration of Look-and-say sequence
lasStep :: String -> String
lasStep [] = []
lasStep  ys@(x:xs) = (intToDigit $ length $ takeWhile(== x) ys) : x : (lasStep  $ dropWhile(== x) ys )

-- helper function for hiding the result of last iteration which we need to make next one.
-- in this case, f function can simply call helper function with starting sequence element 
h :: String -> [String]
h xs = xs : h ys
  where ys = lasStep xs

f :: [String]
f = h "1" 
