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
integer = P.integer lexer
parens = P.parens lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
stringLiteral = P.stringLiteral lexer
charLiteral = P.charLiteral lexer
squares = P.brackets lexer
commaSep1 = P.commaSep1 lexer
commaSep = P.commaSep lexer
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
            |   Str String
            |   StrLabel String
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
            (Str str) -> putStrLn $ "String: " ++ str
            (StrLabel str) -> putStrLn $ "String at: " ++ str
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
    V_Int | V_Void | V_IntPtr | V_Char | V_CharPtr | V_IntArr | V_CharArr
    deriving (Eq)
instance Show Type where
    show V_Int = "int"
    show V_Void = "void"
    show V_IntPtr = "int*"
    show V_Char = "char"
    show V_CharPtr = "char*"
    show V_IntArr = "int[]"
    show V_CharArr = "char[]"
instance Ord Type where
    V_Int <= V_IntPtr = True
    V_Char <= V_Int = True
    V_Char <= V_CharPtr = True
    V_Int <= V_CharPtr = True
    V_CharPtr <= V_CharArr = True
    V_IntPtr <= V_IntArr = True
    V_Int <= V_IntArr = True
    V_Int <= V_CharArr = True
    _ <= _ = False
    
toPtr :: Type -> Type
toPtr V_Int = V_IntPtr
toPtr V_Char = V_CharPtr
toPtr t = error $ "No valid pointer type for: " ++ (show t)

toArr V_Int = V_IntArr
toArr V_Char = V_CharArr
toArr t = error $ "No valid array type for: " ++ (show t)
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
    
declaration_list = do
    lst <- many declaration
    spaces
    eof
    return $ List lst
declaration =
    try(
        do
            f <- fun_declaration
            return f
        )
    <|>
    do
        (VarDec t str) <- var_declaration
        return (GlobalDec t str)
    <?> "declaration"

factor :: Parser Tree
factor = do
            x <- integer
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
strLiteral = do
    str <- stringLiteral
    return $ Str str
    <?> "String literal"
derefrence :: Parser Tree
derefrence = do
    lexeme $ char '*'
    expr <- simple_expression
    return $ Deref expr
    <?> "Derefrence"
addressOf :: Parser Tree
addressOf = do
    lexeme $ char '&'
    v <- var
    return $ Addr v
    <?> "Address of operator"
var :: Parser Tree
var = try(do
        str <- identifier
        return $ Var str)
    <?> "Variable"
varAssign :: Parser Tree
varAssign = try(do
        str <- identifier
        return $ VarAssign str) 
        <?> "Assignment var"
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
    <?> "Comparison operator"
statement = try(call_stmt) <|> expression_statement <|> return_statement <|> selection_statement 
    <|> compound_statement <|> iteration_statement
    <?> "statement"
fun_declaration = 
    do
    t <- type_specifier
    id <- identifier
    
    pars <- parens params
    
    
    stmt <- compound_statement
    return $ FuncDec t id pars stmt
    <?> "Function Declaration"
params = do
    lst <- commaSep param
    return $ List lst
param = 
    do
        t <- type_specifier
        spaces
        id <- identifier
        return $ VarDec t id
    <?> "Parameter"
compound_statement =
    do
        lexeme $char '{'
        locals <- local_declarations
        stmts  <- statement_list
        lexeme $ char '}'
        return $ Compound locals stmts

statement_list = do
    lst <- many statement
    return $ List lst
local_declarations :: Parser Tree
local_declarations = do
    lst <- many var_declaration
    return $ List lst
    
var_declaration = 
    try(do
        t <- type_specifier
        name <- identifier
        semi
        return $ VarDec t name)
    <|>
    do
        t <- type_specifier
        name <- identifier
        
        size <- squares integer
        
        semi
        return $ ArrayDec t name (fromIntegral size)
    <?> "Var declaration"

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
    <?> "type specifier"
    
expression_statement =
    do
        expr <- expression
        semi
        return expr
selection_statement :: Parser Tree
selection_statement = 
    try(do
            lexeme $ reserved "if"
            
            
            condition <- parens expression
            
            stmt <- statement
            lexeme $ reserved "else"
            stmt2 <- statement
            return $ IfElse condition stmt stmt2)
    <|>
        do
            lexeme $ reserved "if"
            
            condition <- parens expression
            
            stmt <- statement
            return $ If condition stmt
    <?> "Conditional statement"

iteration_statement :: Parser Tree
iteration_statement =
    do
        lexeme $ reserved "while"
        
        cond <- parens expression
        
        stmt <- statement
        return $ While cond stmt
return_statement :: Parser Tree
return_statement = 
    do
        lexeme $ reserved "return"
        tree <- expression
        semi
        return $ Return tree
    <?> "return"
    
simple_expression :: Parser Tree
simple_expression =
    try (do
        left <- add_expression
        op <- rel_op
        right <- add_expression
        return $ Operator op left right)
    <|>
        add_expression
    <|> 
        strLiteral
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
    <?> "expression"
call_stmt =
    do
        c <- call
        semi
        return c
call =
    do
        id <- identifier
        
        a <- parens args
        
        return $ FCall id a
    <?> "Function call"
args = do
    lst <- commaSep expression
    return $ List lst
    <?> "args"
run :: Show a => Parser a -> String -> IO ()
run p input
        = case (Parsec.parse p "" input) of
            Left err -> do{ putStr "parse error at "
; print err
                          }
            Right x  -> print x 

parse :: Parser Tree -> String -> IO ( Tree)
parse p input =
    case (Parsec.parse p "" input) of
            Left err -> do
                return $ error $ "Parse error: " ++ (show err)
            Right x  -> return (x)
            

