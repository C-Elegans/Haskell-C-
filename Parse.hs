module Parse where
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle, haskellDef)
import Control.Monad (replicateM_)
import Data.Char (ord)
lexer :: P.TokenParser ()
lexer  = P.makeTokenParser
         $ haskellDef
         {  P.reservedOpNames = ["*","/","+","-", "<",">","=", "<=",">=","!="],
            P.reservedNames = ["return", "if", "else", "while", "int", "void"],
            P.commentLine = "//",
            P.commentStart = "/*",
            P.commentEnd = "*/"
         }
whiteSpace = P.whiteSpace lexer

symbol = P.symbol lexer
natural = P.natural lexer
parens = P.parens lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
stringLiteral = P.stringLiteral lexer
charLiteral = P.charLiteral lexer
lexeme p = do 
    x <- p
    whiteSpace
    return x

data Tree =     Operator OP Tree Tree
            |   Num Int
            |   Var String
            |   VarAssign String
            |   GlobalDec Type String
            |   AnnotatedVar String Type
            |   AnnotatedVarAssign String Type
            |   Assign Tree Tree
            |   List [Tree]
            |   Return Tree
            |   If Tree Tree
            |   IfElse Tree Tree Tree
            |   Compound Tree Tree
            |   EmptyTree
            |   VarDec Type String
            |   ArrayDec Type String Int
            |   FuncDec Type String Tree Tree
            |   FCall String Tree
            |   FCallRet String Tree
            |   While Tree Tree
            |   Deref Tree
            |   Addr Tree
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
            (GlobalDec t str) -> putStrLn ("Global " ++ str ++ "=" ++ (show t))
            (ArrayDec t str sz) -> putStrLn ("Array (" ++ (show t) ++ ") "++ str ++ " [" ++ (show sz) ++ "]") 
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
            (FCallRet id args) ->
                do
                    putStrLn ("Call_r " ++ id ++ "()")
                    spaceTabs col
                    putStrLn "args: "
                    prettyprint_helper (col+1) args
            (While cond tree) ->
                do 
                    putStrLn "While:"
                    prettyprint_helper (col+1) cond
                    spaceTabs col
                    putStrLn "do:"
                    prettyprint_helper (col+1) tree
            (AnnotatedVar str t) ->
                putStrLn $ "Var: " ++ str ++ " (" ++ (show t) ++ ")" 
            (AnnotatedVarAssign str t) ->
                putStrLn $ "Var: " ++ str ++ " (" ++ (show t) ++ ")" 
            (Deref tree) -> do
                putStrLn "Deref:"
                prettyprint_helper (col+1) tree
            (Addr (Var str)) -> do
                putStrLn $ "&" ++ str
            (Addr (AnnotatedVar str t)) -> putStrLn $ "&" ++ str ++ " (" ++ (show t) ++ ")"
data OP = Plus | Minus | Mul | Div | Shl | Shr |Lt | Gt | Eq | Ge | Le | Ne deriving (Show)
data Type =
    V_Int | V_Void | V_IntPtr | V_Char | V_CharPtr
    deriving (Eq)
instance Show Type where
    show V_Int = "int"
    show V_Void = "void"
    show V_IntPtr = "int*"
    show V_Char = "char"
    show V_CharPtr = "char*"
    
toPtr :: Type -> Type
toPtr V_Int = V_IntPtr
toPtr V_Char = V_CharPtr
toPtr t = error $ "No valid pointer type for: " ++ (show t)
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
        (VarDec t str) <- var_declaration
        return (GlobalDec t str))
    

factor :: Parser Tree
factor = do
            x <- natural
            return $ Num $ fromIntegral x
    <|>
        do
            c <- charLiteral
            return $ Num $ ord c
    <|> do 
            x <- parens simple_expression
            return x
    <|> try(do
        (FCall name pars) <- call
        return (FCallRet name pars))
    <|> var
    <|> derefrence
    <|> addressOf
    <?> "factor"
derefrence :: Parser Tree
derefrence = do
    lexeme $ char '*'
    expr <- simple_expression
    return $ Deref expr

addressOf :: Parser Tree
addressOf = do
    lexeme $ char '&'
    v <- var
    return $ Addr v
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
            c <- lexeme $ oneOf "*/"
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
        
        c <- lexeme $ oneOf "+-"
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
statement = try(call_stmt) <|> expression_statement <|> return_statement <|> selection_statement 
    <|> compound_statement <|> iteration_statement

fun_declaration = 
    do
    t <- type_specifier
    id <- identifier
    lexeme $ char '('
    pars <- params
    lexeme $ char ')'
    
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
        lexeme $ char ','
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
        lexeme $char '{'
        locals <- local_declarations
        stmts  <- statement_list
        lexeme $ char '}'
        return $ Compound locals stmts

statement_list =
    do
        stmt <- lexeme $ statement
        (List list) <- statement_list
        return $ List (stmt:list)
    <|> try(do
                return $ List [])
local_declarations :: Parser Tree
local_declarations = 
    do
        vdec <- lexeme $ var_declaration
        (List list) <- local_declarations
        return $ List (vdec:list)
    <|>
    try(do
        return $ List [])

var_declaration = 
    try(do
        t <- type_specifier
        name <- identifier
        lexeme $ char ';'
        return $ VarDec t name)
    <|>
    do
        t <- type_specifier
        name <- identifier
        lexeme $ char '['
        size <- natural
        lexeme $ char ']'
        lexeme $ char ';'
        return $ ArrayDec t name (fromIntegral size)

type_specifier = 
    do
        lexeme $ reserved "int"
        try (do
            lexeme $ char '*'
            return V_IntPtr)
            <|>
            return V_Int
    <|>
    do
        lexeme $ reserved "char"
        try (do
            lexeme $ char '*'
            return V_CharPtr)
            <|>
            return V_Char
    <|>
    do
        lexeme $ reserved "void"
        return V_Void
    
    
expression_statement =
    do
        expr <- expression
        lexeme $ char ';'
        return expr
selection_statement :: Parser Tree
selection_statement = 
    try(do
            lexeme $ reserved "if"
            
            lexeme $ char '('
            condition <- expression
            lexeme $ char ')'
            stmt <- statement
            lexeme $ reserved "else"
            stmt2 <- statement
            return $ IfElse condition stmt stmt2)
    <|>
        do
            lexeme $ reserved "if"
            lexeme $ char '('
            condition <- expression
            lexeme $ char ')'
            stmt <- statement
            return $ If condition stmt
    <?> "Conditional statement"

iteration_statement :: Parser Tree
iteration_statement =
    do
        lexeme $ reserved "while"
        lexeme $ char '('
        cond <- expression
        lexeme $ char ')'
        stmt <- statement
        return $ While cond stmt
return_statement :: Parser Tree
return_statement = 
    do
        lexeme $ reserved "return"
        tree <- expression
        lexeme $ char ';'
        return $ Return tree

    
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
        left <- varAssign
        lexeme $ char '='
        right <- simple_expression
        return $ Assign left right)
    <|>try(do
        lexeme $ char '*'
        left <- simple_expression
        lexeme $ char '='
        right <- simple_expression
        return $ Assign (Deref left) right)
    <|>simple_expression
call_stmt =
    do
        c <- call
        lexeme $ char ';'
        return c
call =
    do
        id <- identifier
        lexeme $ char '('
        a <- args
        lexeme $ char ')'
        return $ FCall id a

args =
    arg_list
    <|>
    do
        return $ List []
arg_list =
    try(do
            expr <- expression
            lexeme $ char ','
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

parse :: Parser Tree -> String -> IO Tree
parse p input =
    case (Parsec.parse p "" input) of
            Left err -> do
                putStr "Parse error at "
                print err
                return (EmptyTree)
            Right x  -> return x
            

