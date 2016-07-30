module Parse where
import Type
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
         {  P.reservedOpNames = ["*","/","+","-", "<",">","=", "<=",">=","!=","<<",">>","&","|","^"],
            P.reservedNames = ["return", "if", "else", "while", "int", "void", "char"],
            P.commentLine = "//",
            P.commentStart = "/*",
            P.commentEnd = "*/"
         }
table = [   [prefix "~" (UnaryOp Not), prefix "-" (UnaryOp Neg), prefix "*" (Deref), prefix "&" (Addr) ],
            [binary "*" (Operator Mul) AssocRight, binary "/" (Operator Div) AssocRight],
            [binary "+" (Operator Plus) AssocRight, binary "-" (Operator Minus) AssocRight],
            [binary "<<" (Operator Shl) AssocRight, binary ">>" (Operator Shr) AssocRight],
            [binary "==" (Operator Eq) AssocRight, binary "!=" (Operator Ne) AssocRight,
                binary "<" (Operator Lt) AssocRight,binary "<=" (Operator Le) AssocRight,
                binary ">" (Operator Gt) AssocRight,binary "==" (Operator Ge) AssocRight],
            [binary "&" (Operator And) AssocRight],
            [binary "^" (Operator Xor) AssocRight],
            [binary "|" (Operator Or) AssocRight]
            
        ]
prefix name fun = Prefix (do reservedOp name; return fun;)
postfix name fun = Postfix (do reservedOp name; return fun;)
binary name fun assoc = Infix (do reservedOp name; return fun;) assoc
expr = buildExpressionParser table factor
data OP = Plus | Minus | Mul | Div | Shl | Shr | And | Or | Xor | Not | Neg |
    Lt | Gt | Eq | Ge | Le | Ne 
    deriving (Show)

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
            |   UnaryOp OP Tree
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
            |   Cast Type Tree
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
            (UnaryOp op left) ->
                do
                    putStrLn $ show op
                    prettyprint_helper (col+1) left
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
            (Cast t exp) -> do
                putStrLn $ "Cast (" ++ (show t) ++ ")"
                prettyprint_helper (col+1) exp

    

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
declaration = do
    t <- type_specifier
    id <- identifier
    a<- (try $ do
            pars <- parens params
            stmt <- compound_statement
            return $ FuncDec t id pars stmt
        <|>
        do
            semi
            return $ VarDec t id
        <|>
        do
            size <- squares integer
            semi
            return $ ArrayDec t id (fromIntegral size))
    return a
    <?> "declaration"


var_declaration = 
    try(do
        t <- lexeme $ type_specifier
        name <- lexeme $ identifier
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
factor :: Parser Tree
factor = do
            x <- integer
            return $ Num $ fromIntegral x
    <|>
        cast
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
    
    
    
    <?> "factor"
cast = do
    t <- parens type_specifier
    exp <- simple_expression
    return $ Cast t exp
    <?> "Cast"
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

statement = try(call_stmt) <|> expression_statement <|> return_statement <|> selection_statement 
    <|> compound_statement <|> iteration_statement
    <?> "statement"

params = try(do
    lst <- commaSep param
    return $ List lst)
    <|>
    do
        reserved "void"
        return $ List []
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
    

type_specifier = do
    p <- primitive_type
    ptrs <- many $ char '*'
    return $ buildType p (length ptrs)
    
buildType t 0 = t
buildType t n = Ptr (buildType t (n-1))

primitive_type = do
        reserved "char"
        return P_Char
    <|>
    do
        reserved "int"
        return P_Int
    <|>
    do
        reserved "void"
        return P_Void

expression_statement =
    do
        expr <- (expression <?> "expression")
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
        expr
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

parse :: Parser Tree -> String -> String -> Either ParseError Tree
parse p filename input = Parsec.parse p filename input
            

