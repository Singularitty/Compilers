import Data.Char

data Token =
      Intlit Int
    | Print
    | Plus
    | Id String
    deriving Show

data AST =
      Leaf Int
    | Sum AST AST
    | PrintAST AST
    | Var AST
    deriving Show

tokenize :: String -> [Token]
tokenize "" = []
tokenize (c:cs)
    | isSpace c = tokenize cs
    | c == '+' = Plus : tokenize cs
    | isDigit c = let (n,cs') = span isDigit cs in Intlit (read (c : n)) : tokenize cs'
    | isAlpha c = let (i,cs') = span isAlphaNum cs 
                  in case  c : i of
                    "print" -> Print : tokenize cs'
                    _ -> Id (c:i) : tokenize cs'
    | otherwise = error $ "unexpected character " ++ show c 


main = print (tokenize "print 1+1")