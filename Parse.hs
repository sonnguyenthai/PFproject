import Expression

-- Get the first item in a token list. It's like the "head" func of list
getFirst :: [Token] -> Token
getFirst [] = TokenEnd
getFirst (t:ts) = t

-- Get the rest of the token list except the first item
takeRest :: [Token] -> [Token]
takeRest [] = error "Nothing to take"
takeRest (t:ts) = ts

expression :: [Token] -> (Expression, [Token])
expression toks = 
   let (termTree, toks') = term toks
   in
      case getFirst toks' of
         (TokenOp op) | elem op [Plus, Minus] -> 
            let (exTree, toks'') = expression (takeRest toks') 
            in (Summary op termTree exTree, toks'')
         _ -> (termTree, toks')

term :: [Token] -> (Expression, [Token])
term toks = 
   let (facTree, toks') = factor toks
   in
      case getFirst toks' of
         (TokenOp op) | elem op [Times, Div] ->
            let (termTree, toks'') = term (takeRest toks') 
            in (Production op facTree termTree, toks'')
         _ -> (facTree, toks')

factor :: [Token] -> (Expression, [Token])
factor toks = 
   case getFirst toks of
      (TokenCon x)     -> (Constant x, takeRest toks)
      (TokenVar str) -> (VariableNode $ Variable str, takeRest toks)
      (TokenOp op) | elem op [Plus, Minus] -> 
            let (facTree, toks') = factor (takeRest toks) 
            in (Unary op facTree, toks')
      TokenLParen      -> 
         let (expTree, toks') = expression (takeRest toks)
         in
            if getFirst toks' /= TokenRParen 
            then error "Missing right parenthesis"
            else (expTree, takeRest toks')
      _ -> error $ "Parse error on token: " ++ show toks

parse :: [Token] -> Expression
parse toks = let (tree, toks') = expression toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'

parseExpression :: String -> Maybe Expression
parseExpression = parse . tokenize