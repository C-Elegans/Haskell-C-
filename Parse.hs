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
         {  P.reservedOpNames = ["*","/","+","-", "<",">","=", "<=",">=","!="],
            P.reservedNames = ["return", "if", "else", "while", "int", "void"]
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
            |   Return Tree
            |   If Tree Tree
            |   IfElse Tree Tree Tree
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
        str <- identifier
        return $ Var str
term :: Parser Tree
term =  try (do 
            left <- factor
            spaces
            c <- oneOf "*/"
            let o = op [c]
            spaces
            right <- term
            return $ Operator o left right)
        <|>
            factor
        <?> "term"
        
add_expression :: Parser Tree
add_expression = 
    try (do
        left <- term
        spaces
        c <- oneOf "+-"
        let o = op [c]
        spaces
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
statement = expression_statement <|> return_statement <|> selection_statement

expression_statement =
    do
        spaces
        expr <- expression
        spaces
        char ';'
        return expr
selection_statement :: Parser Tree
selection_statement = 
    try(do
            reserved "if"
            spaces
            char '('
            spaces
            condition <- expression
            spaces
            char ')'
            spaces
            stmt <- statement
            spaces
            reserved "else"
            spaces
            stmt2 <- statement
            return $ IfElse condition stmt stmt2)
    <|>
        do
            reserved "if"
            spaces
            char '('
            spaces
            condition <- expression
            spaces
            char ')'
            spaces
            stmt <- statement
            return $ If condition stmt
    <?> "Conditional statement"
return_statement :: Parser Tree
return_statement = 
    do
        reserved "return"
        spaces
        tree <- expression
        return $ Return tree

    
simple_expression :: Parser Tree
simple_expression =
    try (do
        left <- add_expression
        spaces
        op <- rel_op
        spaces
        right <- add_expression
        return $ Operator op left right)
    <|>
        add_expression
        
expression :: Parser Tree
expression = 
    try (do
        left <- var
        spaces
        char '='
        spaces
        right <- simple_expression
        return $ Assign left right)
    <|> simple_expression

        
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
            

