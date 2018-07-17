module Homework where
--
import Data.List
import Data.Char
--

-- Task 01
isLeapYear :: Int -> Bool
isLeapYear y = 
  if y `mod` 4 == 0 then
    if y `mod` 100 == 0 then 
      if y `mod` 400 == 0 then
        True
      else
        False
    else
      True
  else
    False

leapList :: [Int]
leapList = [ y | y <- [1996..2017], isLeapYear y] 

-- Task 02
evaluate :: Double -> [Double] -> Double
evaluate x coefs = sum [ (snd c) * x ^ (fst c) | c <- zip [0..] coefs ] 

factorial :: Double -> Double
factorial n = product [1..n] 

maclaurin :: [Double]
maclaurin = [ 1/(factorial i) | i <- [0..] ]

exp' :: Double -> Double
exp' x = evaluate x (take 170 $ maclaurin)

-- Some additional code ...

exp'' :: Int -> Double -> Double
exp'' n x = evaluate x (take n $ maclaurin)
expFirst10Values = [ exp i | i <- [0..10] ] 
expFirst10Values' = [ exp' i | i <- [0..10] ] 
expFirst10ValuesN' n = [ exp'' n i | i <- [0..10] ]


-- Task 03
findItem :: [(String, a)] -> String -> [(String, a)]
findItem l s = [ (fst x, snd x) | x <- l, fst x == s ]

contains :: [(String, a)] -> String -> Bool
contains l s = not $  null $ findItem l s

lookup :: [(String, a)] -> String -> a
lookup l s
  | contains l s = snd $ findItem l s !! 0  -- check if list contains key before retrieving value
  | otherwise = error ("There is no key " ++ s ++ " in list")

insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert l e 
  | contains l $ fst e = l -- do nothing if key exists
  | otherwise = e : l -- if key does not exists, add it as head 

remove :: [(String, a)] -> String -> [(String, a)]
remove l s = [ x | x <- l, s /= fst x ]

update :: [(String, a)] -> String -> a -> [(String, a)]
update l s a  
  | contains l s = Homework.insert (remove l s) (s,a) -- remove old value and add updated one
  | otherwise = l -- do nothing if key does not exists

-- Task 04
cosineSimilarity :: String -> String -> Double
cosineSimilarity t1 t2 = 
  (fromIntegral $  getCrossMatching w1 w2) / (sqrt l1 * sqrt l2)
  where w1 = extractWords t1
        w2 = extractWords t2
        l1 = fromIntegral $ length w1
        l2 = fromIntegral $ length w2

-- Helper functions

-- Gets number of matching words
getCrossMatching :: [String] -> [String] -> Int
getCrossMatching l1 l2 = sum [ 1 | w <- l1, w `elem` l2 ] 
getCrossMatching' t = sum [ 1 | w <- fst t, w `elem` snd t] 

-- Extracts filtered words from text
extractWords :: String -> [String]
extractWords t = [ filterLetters w | w <- words t]

-- Filters letters from input text
filterLetters :: String -> String
filterLetters t = [ c | c <- t, isLetter c ]

