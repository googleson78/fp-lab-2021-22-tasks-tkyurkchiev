{-# LANGUAGE EmptyDataDeriving #-}

module Expr where

import Prelude hiding (lookup, unlines, unwords)

data Expr
  = Var String
  | Val Integer
  | Oper OperType Expr Expr
  | If Expr Expr Expr
  | SumList [Expr]
  | Sum String Expr Expr
  deriving (Show)

data OperType = Plus | Mult
  deriving (Show)

type Context = [(String, Integer)]

extend :: String -> Integer -> Context -> Context
extend x y ctxt = (x, y) : ctxt


lookup :: String -> Context -> Maybe Integer
lookup _ [] = Nothing
lookup s ((x, y) : ctxt)
  | s == x = Just y
  | otherwise = lookup s ctxt

maybeAndThen :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeAndThen Nothing _ = Nothing
maybeAndThen (Just x) f = f x


infixl 1 `maybeAndThen`


traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe _ [] = Just []
traverseListMaybe f (x:xs) = f x `maybeAndThen` (\y -> fmap (y:) (traverseListMaybe f xs))


freeVars :: Expr -> [String]
freeVars (Var x) = [x]
freeVars (Val _) = []
freeVars (Oper _ e1 e2) = freeVars e1 ++ freeVars e2
freeVars (If e1 e2 e3) = freeVars e1 ++ freeVars e2 ++ freeVars e3
freeVars (SumList es) = concatMap freeVars es
freeVars (Sum "i" (Var "i") e2) = freeVars e2
freeVars (Sum "i" e1 e2) = remove "i" $ freeVars e1 ++ freeVars e2
  where remove x xs = filter (/= x) xs
freeVars (Sum _ e1 e2) = freeVars e1 ++ freeVars e2

--sorry for bad homework, i was pushed by assignments and lectures and had no time to do it properly

eval :: Context -> Expr -> Maybe Integer
eval ctxt (Var x) = lookup x ctxt
eval _ (Val x) = Just x
eval ctxt (Oper Plus e1 e2) = eval ctxt e1 `maybeAndThen` (\x -> eval ctxt e2 `maybeAndThen` (\y -> Just (x + y)))
eval ctxt (Oper Mult e1 e2) = eval ctxt e1 `maybeAndThen` (\x -> eval ctxt e2 `maybeAndThen` (\y -> Just (x * y)))
eval ctxt (If cex tex eex) = eval ctxt cex `maybeAndThen` (\x -> if x == 0 then eval ctxt tex else eval ctxt eex)
eval ctxt (SumList es) = traverseListMaybe (eval ctxt) es `maybeAndThen` (Just . sum)
eval ctxt (Sum "i" (Var "i") e2) = eval ctxt e2
eval ctxt (Sum "i" e1 e2) = if eval ctxt e1 < Just 0 then Just 0 else sumMaybe (eval ctxt e2) (eval ctxt (Sum "i" (decremented (eval ctxt e1)) e2))
  where 
    sumMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer
    sumMaybe Nothing Nothing = Nothing
    sumMaybe (Just x) Nothing = Just x
    sumMaybe Nothing (Just y) = Just y
    sumMaybe (Just x) (Just y) = Just (x + y)
    decremented :: Maybe Integer -> Expr
    decremented Nothing = Val 0
    decremented (Just x) = Val (x - 1)

  


intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse val (x:xs) = x : val : intersperse val xs


unwords :: [String] -> String
unwords = concat . intersperse " "


-- hlint wanted me to use "interacalate" so i couldnt use intersperse
unlines :: [String] -> String
unlines [] = []
unlines (x:xs) = x ++ "\n" ++ unlines xs

solvingCompiler :: Bool
solvingCompiler = True

newtype RacketProgram = MkRacketProgram [RacketExpr]

data RacketExpr
  = Name String
  | List [RacketExpr]
  deriving (Show)

printRacketExpr :: RacketExpr -> String
printRacketExpr (Name x) = x
printRacketExpr (List xs) = "(" ++ unwords (map printRacketExpr xs) ++ ")"

printRacketProgram :: Context -> RacketProgram -> String
printRacketProgram ctxt (MkRacketProgram xs) = "#lang racket\n" ++ unlines (map (printRacketExpr . toRacketExpr ctxt) xs)

toRacketExpr :: Context -> RacketExpr -> RacketExpr
toRacketExpr _ (Name x) = Name x
toRacketExpr ctxt (List xs) = List (map (toRacketExpr ctxt) xs)


compileToRacket :: Expr -> RacketExpr
compileToRacket (Var x) = Name x
compileToRacket (Val x) = Name $ show x
compileToRacket (Oper Plus e1 e2) = List [Name "+", compileToRacket e1, compileToRacket e2]
compileToRacket (Oper Mult e1 e2) = List [Name "*", compileToRacket e1, compileToRacket e2]
compileToRacket (If cex tex eex) = List [Name "if", compileToRacket cex, compileToRacket tex, compileToRacket eex]
compileToRacket (SumList es) = List [Name "+" , List (map compileToRacket es)]
compileToRacket (Sum "i" (Var "i") e1) = compileToRacket e1


solvingPartialEval :: Bool
solvingPartialEval = False

partialEval :: Expr -> Expr
partialEval = undefined

solvingSum :: Bool
solvingSum = False
