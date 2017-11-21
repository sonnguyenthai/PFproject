module Expression where

import Data.Char
import Data.Tuple

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)


data Token = TokenOp Operator
           | TokenLParen -- Token of right parenthesis
           | TokenRParen -- Token of left parenthesis
           | TokenVar String -- Token of variable
           | TokenCon Float -- Token of constants 
           | TokenEnd -- Token of end of exp
    deriving (Show, Eq)

data Variable = Variable String deriving (Eq, Show)

{-|
Expression type. We treat an expression as a tree
The grammar description:
Expression <- Term [+-] Expression 
            | Term
Term       <- Factor [*/] Term
            | Factor
Factor     <- Number 
            | Variable 
            | [+-] Factor 
            | '(' Expression ')'
|-}
data Expression = Summary Operator Expression Expression
                | Production Operator Expression Expression
                | Unary Operator Expression
                | Constant Float
                | VariableNode Variable
            deriving Show

-- Store type. It's a association list which contains a list of (variable, value)
data Store = Store [(Variable, Float)] deriving (Show)

-- Convert a char symbol to an Operator
operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div


-- Convert a string to a list of Token
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/" = TokenOp (operator c) : tokenize cs
    | c == '('  = TokenLParen : tokenize cs
    | c == ')'  = TokenRParen : tokenize cs
    | isDigit c = numberize c cs
    | isAlpha c = variablize c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

variablize :: Char -> String -> [Token]
variablize c cs = let (name, cs') = span isAlphaNum cs in
                  TokenVar (c:name) : tokenize cs'

numberize :: Char -> String -> [Token]
numberize c cs = 
   let (digs, cs') = span isDigit cs in
    TokenCon (read (c : digs)) : tokenize cs'

-- Get value of a key from store
getValue :: Variable -> Store -> Float
getValue (Variable key) (Store (x:xs))
    | Variable key == (fst x) = (snd x)
    | otherwise = getValue (Variable key) (Store xs)


eval :: Store -> Expression -> Float
-- Evaluate a Summary expression (+, -)
eval (Store store) (Summary op left right) = 
    let lft = eval (Store store) left
        rgt = eval (Store store) right
    in
        case op of
            Plus  -> lft + rgt
            Minus -> lft - rgt

-- Evaluate a Production expression (*, /)
eval (Store store) (Production op left right) = 
    let lft = eval (Store store) left
        rgt = eval (Store store) right
    in
        case op of
            Times -> lft * rgt
            Div   -> lft / rgt

-- Evaluate a Unary expression
eval (Store store) (Unary op tree) =
    let x = eval (Store store) tree 
    in case op of
        Plus  -> x
        Minus -> -x

-- Evaluate a Constant expression
eval (Store store) (Constant x) = x

-- Evaludate a Variable
eval (Store store) (VariableNode str) = getValue str (Store store)





