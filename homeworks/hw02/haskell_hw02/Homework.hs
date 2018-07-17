module Homework where
--
import Data.List
import Data.Char
--

-- Task 01
toRNA :: String -> String
toRNA xs = [ case x of  
              'G' -> 'C'
              'C' -> 'G'
              'T' -> 'A'
              'A' -> 'U'
              (_) -> error ("Nucleotide " ++ show x ++ " does not exist.")
            | x <- xs ]


-- Task 02
multiply :: Int -> Int -> Int
multiply 1 y = y
multiply x y = y + multiply (x-1) y


divide :: Int -> Int -> Int
divide x y 
  | y == 0    = error "Division by zero!!!"
  | x < y     = 0
  | otherwise = 1 + divide (x-y) y

greatestCD :: Int -> Int -> Int
greatestCD a b
  | b == 0    = a
  | otherwise = greatestCD b $ a `mod` b

-- Task 03

-- basicNumber represents direct "map" with numbers who directly have corresponding word

basicNumber :: Int -> String
basicNumber x 
  | x == 1  = "one"
  | x == 2  = "two"
  | x == 3  = "three"
  | x == 4  = "four"
  | x == 5  = "five"
  | x == 6  = "six"
  | x == 7  = "seven"
  | x == 8  = "eight"
  | x == 9  = "nine"
  | x == 10 = "ten"
  | x == 11 = "eleven"
  | x == 12 = "twelve"
  | x == 13 = "thirteen"
  | x == 14 = "fourteen"
  | x == 15 = "fifteen"
  | x == 16 = "sixteen"
  | x == 17 = "seventee"
  | x == 18 = "eighteen"
  | x == 19 = "nineteen"
  | x == 20 = "twenty"
  | x == 30 = "thirty"
  | x == 40 = "forty"
  | x == 50 = "fifty"
  | x == 60 = "sixty"
  | x == 70 = "seventy"
  | x == 80 = "eighty"
  | x == 90 = "ninety"
  | x == 100 = "hundred"
  | otherwise  = show x

-- complexNumber recursively builds expression for numbers below one thousand
complexNumber :: Int -> String
complexNumber x
  | x >= 100 = (basicNumber (x `div` 100)) ++ " hundred " ++  (complexNumber $ x `mod` 100 )
  | x >= 20  = (basicNumber $ (x `div` 10) * 10 ) ++ " " ++  (complexNumber $ x `mod` 10)
  | x > 0   = (basicNumber x)
  | otherwise = ""


numberToWords :: Int -> String
numberToWords x
  | a > 0 = (complexNumber a ++ " million " ) ++ (numberToWords $ x `mod` 1000000)
  | b > 0 = (complexNumber b ++ " thousand " ) ++ (numberToWords $ x `mod` 1000) 
  | x > 0 = complexNumber x 
  | otherwise = "x"
  where a = x `div` 1000000
        b = x `div` 1000 



-- Task 04
-- This should work because of lazy evaluation. Although calling it will result in infinite recursion !!!.
undefined' :: a
undefined' = undefined'
