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
    | End
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


lookahead :: [Token] -> Maybe Token
lookahead (tk:_) = Just tk
lookahead [] = Nothing

parseS :: [Token] -> AST
parseS tks = case lookahead tks of
  Just Print -> let (_:tks') = tks in PrintAST (parseE tks)
  _ -> error "parsing exception"

parseE :: [Token] -> AST
parseE tks = case lookahead tks of
  Just Id s -> Var 

parse :: [Token] -> AST
parse [] = End



main = print (tokenize "print 1+1")