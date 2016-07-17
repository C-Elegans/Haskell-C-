module Parse where
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle, haskellDef)

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser
         $ haskellDef
         { P.reservedOpNames = ["*","/","+","-", "<",">","=", "<=",">=","!="]
         }
whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
natural = P.natural lexer
parens = P.parens lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

data Tree =     Operator OP Tree Tree
            |   Num Integer
            |   Var String
            |   Assign Tree Tree
            |   List [Tree]
            deriving (Show)
data OP = Plus | Minus | Mul | Div |Lt | Gt | Eq | Ge | Le | Ne deriving (Show)
op :: String -> OP
op c 
    | c == "+" = Plus
    | c == "-" = Minus
    | c == "*" = Mul
    | c == "/" = Div
    | c == "<" = Lt
    | c == ">" = Gt
    | c == "==" = Eq
    | c == "!=" = Ne
    | c == "<=" = Le
    | c == ">=" = Ge
    | otherwise = error $ "Invalid operator: " ++ c


factor :: Parser Tree
factor = do
            x <- many1 digit <?> ""
            return $ Num $ read x 
        
    <|> do 
            x <- parens simple_expression
            return x
    <|> var
    <?> "factor"
var :: Parser Tree
var = do
        str <- many1 letter
        return $ Var str
term :: Parser Tree
term =  try (do 
            left <- factor
            c <- oneOf "*/"
            let o = op [c]
            right <- term
            return $ Operator o left right)
        <|>
            factor
        <?> "term"
        
add_expression :: Parser Tree
add_expression = 
    try (do
        left <- term
        c <- oneOf "+-"
        let o = op [c]
        right <- add_expression
        return $ Operator o left right)
    <|> 
        term

rel_op :: Parser OP
rel_op =
        try (do {str <- string "<="; return $ op str})
    <|> try (do {str <- string ">="; return $ op str})
    <|> do {str <- string "!="; return $ op str}
    <|> do {str <- string "=="; return $ op str}
    <|> do {str <- string ">"; return $ op str}
    <|> do {str <- string "<"; return $ op str}
    
simple_expression :: Parser Tree
simple_expression =
    try (do
        left <- add_expression
        op <- rel_op
        right <- add_expression
        return $ Operator op left right)
    <|>
        add_expression
        
expression :: Parser Tree
expression = 
    try (do
        left <- var
        char '='
        right <- simple_expression
        return $ Assign left right)
    <|> simple_expression
expr_list :: Parser Tree
expr_list =
    do
        expr <- expression
        char ';'
        (List list) <- expr_list
        
        return $ List (expr:list)
    <|> try(do
            return $ List [])
        
run :: Show a => Parser a -> String -> IO ()
run p input
        = case (Parsec.parse p "" input) of
            Left err -> do{ putStr "parse error at "
; print err
                          }
            Right x  -> print x 

parse :: Parser Tree -> String -> Tree
parse p input =
    case (Parsec.parse p "" input) of
            Left err -> Num (-1)
            Right x  -> x
            

