module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Expression
  = Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop
  = Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement
  = Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Part 01 -----------------------------------------

-- st s = _i
extend :: State -> String -> Int -> State
extend st s i = \k -> if s == k then i else st k 

empty :: State
empty = \k -> 0 

-- Part 02 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt b = if b == True then 1 else 0

evalE :: State -> Expression -> Int
evalE st (Var s)            = st s 
evalE st (Val i)            = i
evalE st (Op e1 Plus e2)    = (evalE st e1) + (evalE st e2)
evalE st (Op e1 Minus e2)   = (evalE st e1) - (evalE st e2)
evalE st (Op e1 Times e2)   = (evalE st e1) * (evalE st e2)
evalE st (Op e1 Divide e2)  = (evalE st e1) + (evalE st e2)

evalE st (Op e1 Gt e2)       = boolToInt $ (evalE st e1) > (evalE st e2)
evalE st (Op e1 Ge e2)       = boolToInt $ (evalE st e1) >= (evalE st e2)
evalE st (Op e1 Lt e2)       = boolToInt $ (evalE st e1) < (evalE st e2)
evalE st (Op e1 Le e2)       = boolToInt $ (evalE st e1) <= (evalE st e2)
evalE st (Op e1 Eql e2)      = boolToInt $ (evalE st e1) == (evalE st e2)

-- Part 03 -----------------------------------------

data DietStatement
  = DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e)     = DAssign s e 
desugar (Incr s)         = DAssign s (Op (Var s) Plus (Val 1))
desugar (If e1 s1 s2)    = DIf e1 (desugar s1) (desugar s2)
desugar (While e1 s)     = DWhile e1 (desugar s)
desugar (For s1 e s2 s3) = 
      DSequence (desugar s1) (DWhile e (DSequence (desugar s3) (desugar s2))) 
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar Skip             = DSkip

-- Part 04 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s e) = extend st s $ evalE st e
evalSimple st (DIf e ds1 ds2) = 
                if evalE st e == 0 
                then evalSimple st ds2
                else evalSimple st ds1
evalSimple st (DWhile e ds) = 
                if evalE st e == 0 
                then st
                else evalSimple st (DSequence ds (DWhile e ds)) 
evalSimple st (DSequence ds1 ds2) = evalSimple (evalSimple st ds1) ds2
evalSimple st DSkip = st

run :: State -> Statement -> State
run st s = evalSimple st $ desugar s 

-- Part 05 -----------------------------------------

parse :: String -> Maybe Statement
parse = undefined

-- Programs ----------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input
  for (Out := 1; In > 0; In := In - 1) {
      Out := In * Out
  }
-}

factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{-
  Calculate the floor of the square root of the input
  B := 0;
  while (A >= B * B) {
      B++
  };
  B := B - 1
-}

squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{-
  Calculate the nth Fibonacci number

  F0 := 1;
  F1 := 1;

  if (In == 0) {
      Out := F0
  } else {
      if (In == 1) {
          Out := F1
      } else {
          for (C := 2; C <= In; C++) {
              T  := F0 + F1;
              F0 := F1;
              F1 := T;
              Out := T
          }
      }
  }

-}

fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
