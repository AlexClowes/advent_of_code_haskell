import Control.Monad.State
import Data.Bits
import Data.List (isPrefixOf)
import Data.Map (Map, fromList, (!), insert)
import Data.Maybe
import Text.Read (readMaybe)

import Debug.Trace

split :: Eq a => [a] -> [a] -> ([a], [a])
split sub lst = iter lst [] where
    iter lst@(x:xs) acc
        | isPrefixOf sub lst = (reverse acc, drop (length sub) lst)
        | otherwise = iter xs (x:acc)

type Env = Map Name Expr

type Name = String
data Expr = Integer Int | Variable Name | UnaryExpr UnaryOp Expr | BinaryExpr BinaryOp Expr Expr
type UnaryOp = Int -> Int
type BinaryOp = Int -> Int -> Int

parse :: String -> Env
parse = fromList . map parseLine . lines

parseLine :: String -> (Name, Expr)
parseLine line =
    let (exprString, name) = split " -> " line
    in (name, parseExpr exprString)

parseExpr :: String -> Expr
parseExpr exprString = 
    let exprTokens = words exprString
    in case length exprTokens of
        1 -> let string = head exprTokens
                 asInt = readMaybe string :: Maybe Int
             in if isJust asInt then Integer (fromJust asInt) else Variable string
        2 -> let [opString, argString] = exprTokens
                 op
                    | opString == "NOT" = complement
             in UnaryExpr op (parseExpr argString)
        3 -> let [argString1, opString, argString2] = exprTokens
                 op
                     | opString == "AND" = (.&.)
                     | opString == "OR" = (.|.)
                     | opString == "LSHIFT" = shift
                     | opString == "RSHIFT" = (\x y -> shift x (-y))
             in BinaryExpr op (parseExpr argString1) (parseExpr argString2)

eval :: Expr -> State Env Int
eval (Integer n) = return n
eval (Variable name) = do
    env <- get
    val <- eval (env ! name)
    put (insert name (Integer val) env)
    return val
eval (UnaryExpr op expr) = do
    arg <- eval expr
    return $ op arg
eval (BinaryExpr op expr1 expr2) = do
    arg1 <- eval expr1
    arg2 <- eval expr2
    return $ op arg1 arg2

main = do
    instructions <- readFile "instructions.txt"
    let env = parse instructions
    let part1 = evalState (eval (Variable "a")) env
    print part1
    let part2 = evalState (eval (Variable "a")) (insert "b" (Integer part1) env)
    print part2
