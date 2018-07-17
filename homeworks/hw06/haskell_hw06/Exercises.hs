{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
import Data.List
import Data.Char
import Control.Exception
import Data.Char
import Data.List
import qualified Data.Map as M
import Control.Monad
import System.IO
import System.Directory
import System.IO.Error
import System.Environment
import System.FilePath
import System.Random
import System.FilePath
import System.Exit
import Data.Set as Set(fromList, member,Set) 

--

{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 10-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs

-- EXERCISE 01 =======================================================================
{-
  1.2.
  - Define a function
    parentCheck :: Person2 -> Bool
    that checks whether the given person is one of the children of its parents.
-}

data Sex = Male | Female deriving (Show,Read,Ord,Eq)

data Person2 = Person2 {
   personId2 :: String,
   forename2 :: String,
   surname2  :: String,
   sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
   mother2   :: Maybe Person2,
   father2   :: Maybe Person2,
   partner2  :: Maybe Person2,
   children2 :: [Person2] } deriving (Show,Read,Eq,Ord)


parentCheck :: Person2 -> Bool
parentCheck p2 = (personId2 p2) `elem` nub ( (pairsChildren2 $ father2 p2) 
                              ++ (pairsChildren2 $ mother2 p2))
   where pairsChildren2 p = map (personId2) $ ( maybe [] children2 p)

{-
  1.3.
  - Define a function
    sister :: Person2 -> Maybe Person2
    that returns the sister of a person, if such exists.
-}

getFemaleSiblings :: Person2 -> [Person2]
getFemaleSiblings p = 
  filter (\p2 -> sex2 p2 == Female && personId2 p /= personId2 p2 ) 
         (( maybe [] children2 $ father2 p) 
      ++ (maybe [] children2 $ mother2 p))


sister :: Person2 -> Maybe Person2
sister p
  | null fsiblings == True    = Nothing
  | otherwise                 = Just $ head fsiblings
  where fsiblings = getFemaleSiblings p 


{-
  1.4.
  - Define a function that returns all descendants of a person.
    descendant :: Person2 -> [Person2]
-}

descendChildren :: [Person2] -> [Person2]
descendChildren []     = [] 
descendChildren (x:xs) =  x : (descendChildren $ children2 x) ++ descendChildren xs 

descendant :: Person2 -> [Person2]
descendant p = descendChildren $ children2 p

-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define
    listHead :: MyList a -> Maybe a
-}

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Eq,Ord)
infixr 5 -+-
(-+-) = Cons

listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (x `Cons` xs) = Just x

{-
  2.2.
  - Define a function that works like 'map' but works on a 'MyList' type:
    listMap :: (a -> b) -> MyList a -> MyList b
-}

listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty = Empty
listMap f (x `Cons` xs) = f x `Cons` listMap f xs

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define a function
    treeMax :: Ord a => Tree a -> a
    that finds the maximum element in a tree. Return an error if the tree is
    empty.
-}

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Eq,Show)

treeMax :: Ord a => Tree a -> a
treeMax Null =  error "Empty tree"
treeMax (Node y Null Null) = y 
treeMax (Node y left right) 
   | left == Null   = max y $ treeMax right 
   | right == Null  = max y $ treeMax left 
   | otherwise      = max (max y $ treeMax left) $ treeMax right


{-
  3.2.
  - Define a function
    treeToList :: Ord a => Tree a -> [a]
    that will collect in a list all elements from inner nodes of a tree by doing
    an in-order (left-root-right) traversal.
-}

treeToList :: Ord a => Tree a -> [a]
treeToList (Node y Null Null) = y : []   
treeToList (Node y left right) 
  | left == Null   = y : treeToList(right) 
  | right == Null  = treeToList(left) ++ [y]
  | otherwise      = treeToList(left)  ++ [y] ++ treeToList(right)


{-
  3.3.
  - Define a function to prune the tree at a given level (root has level 0).
    levelCut :: Int -> Tree a -> Tree a
-}

levelCut :: Int -> Tree a -> Tree a
levelCut 0 (Node y _ _ )        = Node y Null Null
levelCut n (Node y Null r)      = Node y Null (levelCut (n-1) r)
levelCut n (Node y l Null )     = Node y (levelCut (n-1) l) Null
levelCut n (Node y l r )        = Node y (levelCut (n-1) l) (levelCut (n-1) r)


-- EXERCISE 04 =======================================================================
{-
  4.1.
  - Define a function that converts a list into a sorted tree.
    listToTree :: Ord a => [a] -> Tree a
-}

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t


listToTree :: Ord a => [a] -> Tree a
listToTree xs = foldr (\a tree -> treeInsert a tree) Null xs 

{-
  4.2.
  - Using 'listToTree' and 'treeToList' defined previously, define these two 
    functions, define:
    sortAndNub :: Ord a => [a] -> [a]
-}

sortAndNub :: Ord a => [a] -> [a]
sortAndNub xs = treeToList $ listToTree xs

-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define an 'Eq' instance for the 'Weekday' type that works like (==), except
    that two Fridays are never identical.
-}

data Weekday = 
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = False
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False   
{-
  5.2.
  - Define 'Person' as an instance of 'Show' type class so that instead of the
    values of partners and children only the respective person names are shown,
    which will enable the print out of an infinite structure of this type.
-}

data Person = Person
  { idNumber :: String
  , forename :: String
  , surname  :: String
  , sex      :: Sex
  , age      :: Int
  , partner  :: Maybe Person
  , children :: [Person]
  } deriving (Read,Eq,Ord)


instance Show Person where
  show p =  forename p    
{-LECTURE 11-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

-- EXERCISE 01 =======================================================================

{- DON'T FORGET TO WRITE TYPE SIGNATURES-}

{-
  1.1.
  - Define a 'main' function that reads in two strings and prints them out
    concatenated and reversed.
-}

main = do
  putStrLn "Enter first string"
  s1 <- getLine
  putStrLn "Enter second string"
  s2 <- getLine
  putStrLn $ reverse s1 ++ reverse s2


{-
  1.2.
  - Write a function 'threeNumbers' that reads in three numbers and prints out
    their sum.
-}

threeNumbers = do
  putStrLn "Enter three numbers"
  n1 <- getLine
  n2 <- getLine
  n3 <- getLine
  putStrLn $ show ( (read n1 :: Int) + (read n2 :: Int) + (read n3 :: Int))


-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define a function 'threeStrings' that reads in three strings and outputs them
    to the screen as one string, while it returns its total length.
    treeStrings :: IO Int
-}

treeStrings :: IO Int
treeStrings = do
  putStrLn "Enter three strings"
  s1 <- getLine
  s2 <- getLine
  s3 <- getLine
  putStrLn $ s1 ++ s2 ++ s3
  return $ sum $ map length [s1,s2,s3]

{-
  2.2.
  - Define a function 'askNumber9' that reads in a number and returns that number
    converted into an 'Int'. Input should be repeated until the user enters a
    number (a string containing only digits).
      askNumber9 :: IO Int
-}

isValidNumber :: String -> Bool
isValidNumber xs = foldr (\x z -> z && isDigit x) True xs

askNumber9 :: IO Int
askNumber9 = do
  putStrLn "Enter number"
  n <- getLine
  if isValidNumber n then return (read n :: Int) else askNumber9


{-
  2.3.
  - Define a function 'askUser m p' that returns an action that prints out 'm',
    reads in a string from the input, repeats the input until the input
    string satisfies the function 'p', and then returns the input string.
      askUser :: String -> (String -> Bool) -> IO String
  - Generalize this function to
      askUser' :: Read a => String -> (String -> Bool) -> IO a
-}

askUser :: String -> (String -> Bool) -> IO String
askUser m p = do
  putStrLn m
  input <- getLine
  if p input then return input else askUser m p

askUser' :: Read a => String -> (String -> Bool) -> IO a
askUser' m p = do
  putStrLn m
  input <- getLine
  if p input then return $ read input else askUser' m p

{-
  2.4.
  - Define a function that reads in strings until the user inputs an empty
    string, and then returns a list of strings received as input.
      inputStrings :: IO [String]
-}

inputStrings :: IO [String]
inputStrings = do
  input <- getLine
  if null input 
    then return []
    else do
      xs <- inputStrings
      return (input:xs)

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define a function that reads in a number, then reads in that many
    strings, and finally prints these strings in reverse order.
-}

readStrings = do
  n <- getLine
  let nb = (read n :: Int )
  ys <- replicateM nb getLine
  forM_ ys (print . reverse)

{-
  3.2.
  - Give recursive definitions for 'sequence' and 'sequence_'.
-}

sequence' :: Monad m => [m a] -> m [a]
sequence' []     = return []
sequence' (x:xm) = do
  v <- x
  vs <- sequence' xm
  return (v : vs)

sequence'_ :: Monad m => [m a] -> m () 
sequence'_ []     = return ()
sequence'_ (x:xm) = do
  v <- x
  vs <- sequence'_ xm
  return ()

{-
  3.3.
  - Give a recursive definitions for 'mapM' and 'mapM_'.
-}

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f xs = sequence $ map f xs

mapM'_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM'_ f xs = sequence'_ $ map f xs

{-
  3.4.
  - Define a function that prints out the Pythagorean triplets whose all sides
    are <=100. Every triplet should be in a separate line.
-}

pyTriplet :: IO ()
pyTriplet = forM_ [ (a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100]] $
                  \t@(a,b,c) -> when (a*a + b*b == c*c ) $ putStrLn $ show t

-- EXERCISE 04 =======================================================================
{-
  4.1.
  - Define a function that removes from standard input every second line and
    prints the result to standard output.
      filterOdd :: IO ()
-}

filterOdd :: IO ()
filterOdd = interact (unlines . map snd . filter ( odd . fst) . zip [1..] . lines )

{-
  4.2.
  - Define a function that prefixes each line from standard input with a line
    number (number + space).
      numberLines :: IO ()
-}

numberLines :: IO ()
numberLines = interact ( unlines . map (\(n,l) -> show n ++ " " ++ l) . zip [1..] . lines)

{- 4.3.
  - Define a function to remove from standard input all words from a given set of
    words.
      filterWords :: Set String -> IO ()
-}

filterWords :: Set String -> IO ()
filterWords s = interact ( unlines . filt . lines)
  where filt xs = [ unwords $ foldr (\x z -> if not $ x `member` s then (x:z) else z) [] $ words x | x <- xs ]


-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define a function
    wc :: FilePath -> IO (Int, Int, Int)
    that counts the number of characters, words, and lines in a file.
-}

wc :: FilePath -> IO (Int, Int, Int)
wc f = withFile f ReadMode $ \h -> do
  s <- hGetContents h
  let ls = lines s
  let ws = concatMap words ls
  putStrLn $ unwords ws 
  return (foldr (\x z -> z + length x) 0 ws, length ws, length ls )


{-
  5.2. 
  - Define a function
    copyLines :: [Int] -> FilePath -> FilePath -> IO ()
    that copies given lines from the first file into the second.
-}

copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines is f1 f2 = withFile f1 ReadMode $ \h -> do
  s <- hGetContents h
  let ls = map snd . filter (\(i,_) -> i`elem` is) . zip [1..] $  lines s
  withFile f2 WriteMode $ \h2 -> do
    forM_ ls $ hPutStrLn h2


-- EXERCISE 06 =======================================================================
{-
  6.1.
  - Define a function
      wordTypes :: FilePath -> IO Int
    to compute the number of distinct words in the given file.
-}

wordTypes :: FilePath -> IO Int
wordTypes f = do
  s <- readFile f
  return (length . nub $ words s)


{-
  6.2.
  - Define a function 
      diff :: FilePath -> FilePath -> IO ()
    that takes two file names, compares their corresponding lines, and then
    outputs to standard output all lines in which the files differ. Lines should 
    be printed one below the other, prefixed with "<" for the first and ">" for
    the second file.
-}

formatLines :: [String] -> [String] -> IO ()
formatLines []     [] = return ()
formatLines (l:ls) [] = do
  putStrLn $ "< " ++ l  
  formatLines ls []
formatLines [] (l:ls) = do
  putStrLn $ "> " ++ l  
  formatLines [] ls
formatLines (l1:ls1) (l2:ls2) = do
  when (l1 /= l2) $ ( do 
    putStrLn $ "< " ++ l1
    putStrLn $ "> " ++ l2 )
  formatLines ls1 ls2

diff :: FilePath -> FilePath -> IO ()
diff f1 f2 = do
  s1 <- readFile f1
  s2 <- readFile f2
  formatLines (lines s1) (lines s2)


{-
  6.3.
  - Define a function
      removeSpaces :: FilePath -> IO () 
    that removes trailing spaces from all lines in the given file.
    The function should change the original file.
-}

clearSpaces :: String -> String
clearSpaces s 
  | null s || last s /= ' '     = s
  | otherwise                   = clearSpaces $ init s

removeSpaces :: FilePath -> IO ()
removeSpaces f = do
  (ft,ht) <- openTempFile "" f
  s <- readFile f
  hPutStr ht . unlines . map clearSpaces $ lines s
  hClose ht
  renameFile ft f


-- EXERCISE 07 =======================================================================
{-
  7.1.
  - Define a function
      fileHead :: IO ()
    that prints the first 'n' lines from a file. The name of the file and the
    number of lines are specified at the command line, e.g.:
      filehead -5 input.txt
    If the number of lines is missing, default to 10. If file name is missing,
    read from the standard input. If the file doesn't exist, print an error
    message and exit with failure using 'exitFailure' from 'System.Exit'.
-}

fileExists :: FilePath -> IO Handle
fileExists f = do
  e <- doesFileExist f
  if e then openFile f ReadMode 
       else do
         putStrLn "File does not exist"
         exitFailure

fileHead :: IO ()
fileHead = do
  xs <- getArgs
  (n,h) <- case xs of
    []    -> return (10 :: Int, stdin)
    (f:[]) -> do
          h <- fileExists f
          return (10 :: Int, h)
    (n:f:_) -> do
          h <- fileExists f
          return (read n, h)

  s <- hGetContents h
  putStr . unlines . take n $ lines s 
  hClose h

{-
  7.2.
  - Define a function
      sortFiles :: IO ()
    that sorts lines from multiple files and prints them to standard output.
    File names are provided at the command line.
    "sortFiles file1.txt file2.txt file3.txt"
    If any of the files does not exist, print an error message.
-}

sortFiles :: IO ()
sortFiles = do
  xs <- getArgs
  lss <- forM xs (\x -> do
              e <- doesFileExist x
              if e then do
                  h <- openFile x ReadMode
                  s <- hGetContents h
                  return (lines s)
                    else do
                  putStrLn ("*** File " ++ x ++ " does not exists")
                  return [""]
                  )
 
  putStrLn $ unlines $ sort $ concat lss
  return ()

-- EXERCISE 08 =======================================================================
{-
  8.1.
  - Define your own implementation of
      randoms' :: (RandomGen g, Random a) => g -> [a]
-}

-- For some reason this does not work in Exercises file, but normally compiles in lecture file...
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' = undefined
-- randoms' g = (fst g') : (randoms' $ snd g')
--  where g' = random g

{-
  8.2.
  - Define a function
      randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
    that returns a list of randomly generated integer coordinates from within a
    given interval.
      randomPositions 0 10 0 10 => [(2,1),(4,3),(7,7),...
-}

randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
randomPositions x1 x2 y1 y2 = do
  g <- getStdGen
  g2 <- newStdGen
  let xs = randomRs (x1,x2) g
      ys = randomRs (y1,y2) g2
  return (zip xs ys)
