module Parse where
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle, haskellDef)
import Control.Monad (replicateM_)
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
            |   VarAssign String
            |   Assign Tree Tree
            |   List [Tree]
            |   Return Tree
            |   If Tree Tree
            |   IfElse Tree Tree Tree
            |   Compound Tree Tree
            |   EmptyTree
            |   VarDec Type String
            |   FuncDec Type String Tree Tree
            |   FCall String Tree
            deriving (Show)
spaceTabs n = replicateM_ n (putStr "    ")

prettyprint_tree tree =
    prettyprint_helper 0 tree
    
prettyprint_helper col tree =
    do
        spaceTabs col
        case tree of
            (Num x) -> putStrLn $ show x
            (Operator op left right) ->
                do
                    putStrLn $ show op
                    prettyprint_helper (col+1) left
                    prettyprint_helper (col+1) right
            (Var x) -> putStrLn ("Var: " ++x)
            (VarAssign str) -> putStrLn ("VarAssign: " ++ str)
            (Assign left right) ->
                do
                    putStrLn "Assign"
                    prettyprint_helper (col+1) left
                    prettyprint_helper (col+1) right
            (List list) ->
                do
                    putStrLn "List"
                    mapM_ (prettyprint_helper (col+1)) list
            (Return tree) ->
                do
                    putStrLn "Return"
                    prettyprint_helper (col+1) tree
            (If left right) ->
                do
                    putStrLn "If"
                    prettyprint_helper (col+1) left
                    spaceTabs col
                    putStrLn "Then: "
                    prettyprint_helper (col+1) right
            (IfElse cond left right) ->
                do
                    putStrLn "If"
                    prettyprint_helper (col+1) cond
                    spaceTabs col
                    putStrLn "Then: "
                    prettyprint_helper (col+1) left
                    spaceTabs col
                    putStrLn "else: "
                    prettyprint_helper (col+1) right
            (Compound decls stmts) ->
                do
                    putStrLn "Block"
                    prettyprint_helper (col+1) decls
                    prettyprint_helper (col+1) stmts
            (EmptyTree) -> putStrLn "Empty"
            (VarDec t str) -> putStrLn ("VarDec " ++ str ++ " = " ++ (show t))
            (FuncDec t id left right) ->
                do
                    putStrLn ("Func " ++ id ++ " -> " ++ (show t))
                    spaceTabs col
                    putStrLn "Pars: "
                    prettyprint_helper (col+1) left
                    spaceTabs col
                    putStrLn "Body: "
                    prettyprint_helper (col+1) right
            (FCall id args) ->
                do
                    putStrLn ("Call " ++ id ++ "()")
                    spaceTabs col
                    putStrLn "args: "
                    prettyprint_helper (col+1) args
            
data OP = Plus | Minus | Mul | Div |Lt | Gt | Eq | Ge | Le | Ne deriving (Show)
data Type =
    V_Int | V_Void
instance Show Type where
    show V_Int = "int"
    show V_Void = "void"
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

declaration_list =
    do
        decl <- declaration
        spaces
        (List list) <- declaration_list
        return $ List (decl:list)
    <|>
    try( do
        return $ List [] )
declaration =
    try(
        do
            f <- fun_declaration
            return f
        )
    <|>
    try(do
        vdec <- var_declaration
        return vdec)
    

factor :: Parser Tree
factor = do
            x <- many1 digit <?> ""
            return $ Num $ read x 
        
    <|> do 
            x <- parens simple_expression
            return x
    <|> try( do
                c <- call
                return c)
    <|> var
    
    <?> "factor"
var :: Parser Tree
var = try(do
        str <- identifier
        return $ Var str)
varAssign :: Parser Tree
varAssign = try(do
        str <- identifier
        return $ VarAssign str)  
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
statement = expression_statement <|> return_statement <|> selection_statement <|> compound_statement

fun_declaration = 
    do
    t <- type_specifier
    spaces
    id <- identifier
    spaces
    char '('
    spaces
    pars <- params
    spaces
    char ')'
    spaces
    stmt <- compound_statement
    return $ FuncDec t id pars stmt

params = try (param_list) <|> 
    do
        reserved "void"
        return $ List []
param_list =
    try(do
        p <- param
        spaces
        char ','
        spaces
        (List params) <- param_list
        return $ List (p:params))
    <|>
        do
            p <- param
            return $ List [p]
param = 
    do
        t <- type_specifier
        spaces
        id <- identifier
        return $ VarDec t id
compound_statement =
    do
        char '{'
        spaces
        locals <- local_declarations
        spaces
        stmts  <- statement_list
        spaces
        char '}'
        return $ Compound locals stmts

statement_list =
    do
        stmt <- statement
        spaces
        (List list) <- statement_list
        return $ List (stmt:list)
    <|> try(do
                return $ List [])
local_declarations :: Parser Tree
local_declarations = 
    do
        vdec <- var_declaration
        spaces
        (List list) <- local_declarations
        return $ List (vdec:list)
    <|>
    try(do
        return $ List [])

var_declaration = 
    try(do
        t <- type_specifier
        spaces
        name <- identifier
        spaces
        char ';'
        return $ VarDec t name)
type_specifier = 
    do
        reserved "int"
        return V_Int
    <|>
    do
        reserved "void"
        return V_Void
    
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
        spaces
        char ';'
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
        left <- varAssign
        spaces
        char '='
        spaces
        right <- simple_expression
        return $ Assign left right)
    <|> simple_expression
call =
    do
        id <- identifier
        spaces
        char '('
        spaces
        a <- args
        spaces
        char ')'
        return $ FCall id a

args =
    arg_list
    <|>
    do
        return $ List []
arg_list =
    try(do
            expr <- expression
            spaces
            char ','
            spaces
            (List list) <- arg_list
            return $ List (expr:list))
    <|>
        do
            expr <- expression
            return $ List [expr]
        
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
            

