module TempCodegen where
import Instructions
import Parse hiding (Eq, Ne, Gt, Ge, Lt, Le, Shl, Shr)
import qualified Parse
import Debug.Trace
import Control.Monad.State
import qualified Data.Map as M
type SymTab = M.Map String Location

data Location = Global | Local Int
    deriving (Show)
codegen :: [(Tree,SymTab)] -> [(String,Location)] -> String -> [Instruction]
codegen trees globals filename =
    fst $ runState (doCodegen trees globals) (filename,0)

doCodegen :: [(Tree,SymTab)] -> [(String,Location)] -> State (String,Int) [Instruction]
doCodegen (pair:rest) globals = do
    let (func,tab) = pair
    let newTab = M.union tab (M.fromList globals) 
    res <- codegen_helper func newTab
    code <- doCodegen (rest) globals
    return (res ++ code)
doCodegen [] globals = return []

labelSuffix :: State (String,Int) String
labelSuffix = do
    (name,count) <- get
    put (name,count+1)
    return ("_" ++ (show count) ++ "_" ++ name)


codegen_helper :: Tree -> SymTab -> State (String,Int) [Instruction]
codegen_helper (Num x) _ =
    return [Inst_I Push (Const x)]

codegen_helper (Operator op left right) t = do
    lft <- codegen_helper left t
    rgt <- codegen_helper right t
    let operation = case op of
            Plus ->
                [Inst_RR Add R0 R1]
            Minus ->
                [Inst_RR Sub R0 R1]
            Parse.Shl ->
                [Inst_RR Shl R0 R1]
            Parse.Shr -> [Inst_RR Shr R0 R1]
            Mul -> [Inst_JmpI Call Al (Label "mul")]
            Div -> error "Div unsupported"
            Parse.Eq -> [Inst_RR Cmp R0 R1, Inst_Jmp Set Eq R0]
            Parse.Ne -> [Inst_RR Cmp R0 R1, Inst_Jmp Set Ne R0]
            Parse.Gt -> [Inst_RR Cmp R0 R1, Inst_Jmp Set G R0]
            Parse.Ge -> [Inst_RR Cmp R0 R1, Inst_Jmp Set Ge R0]
            Parse.Lt -> [Inst_RR Cmp R0 R1, Inst_Jmp Set L R0]
            Parse.Le -> [Inst_RR Cmp R0 R1, Inst_Jmp Set Le R0]
    return (lft ++ rgt ++ [Inst_R Pop R1, Inst_R Pop R0] ++ operation ++ [Inst_R Push R0])

codegen_helper (AnnotatedVar str t) tab
    |t == V_CharArr || t == V_IntArr = 
    let loc = M.lookup str tab
    in case loc of
        (Just (Local loc)) ->
            return [Inst_RR Mov R0 R6, Inst_RI Sub R0 (Const loc), Inst_R Push R0]
        (Just Global) ->
            return [Inst_RI Mov R0 (Label str), Inst_R Push R0]
        Nothing -> return $ error $ "Undefined vairable: " ++ str
codegen_helper (AnnotatedVar str t) tab = 
    let loc = M.lookup str tab
        bf = if t == V_Char then Byte else Word
    in case loc of
        (Just (Local loc)) ->
            return [Inst_MemI Ld R0 R6 (Const (-loc)) bf Displacement, Inst_R Push R0]
        (Just Global) ->
            return [Inst_MemI Ld R0 R0 (Label str) bf Constant, Inst_R Push R0] 
        Nothing ->
            return (error $ "Undefined variable: " ++ str)

codegen_helper (Assign (Deref left) right) tab = do
    let t = getType left
    let bf = if t == V_CharArr || t == V_CharPtr then Byte else Word
        
    lft <- codegen_helper left tab
    rgt <- codegen_helper right tab
    return (lft ++rgt++[Inst_R Pop R1, Inst_R Pop R0, Inst_Mem St R0 R1 bf])
codegen_helper (Assign (AnnotatedVarAssign str t) expr) tab = do
    code <- codegen_helper expr tab
    let vloc = M.lookup str tab
    let bf = if t == V_Char then Byte else Word
    case vloc of
        (Just (Local loc)) ->
            return (code ++ [Inst_R Pop R0, Inst_MemI St R6 R0 (Const (-loc)) bf Displacement])
        (Just Global) ->
            return (code ++ [Inst_R Pop R0, Inst_MemI St R0 R0 (Label str) bf Constant])
        (Nothing) -> return (error $ "Undefined variable: " ++ str)

codegen_helper (FuncDec t str vs stmts) tab = do
    let (Just (Local spsub)) = M.lookup " count" tab
        movs = movPars vs 0 tab
    code <- codegen_helper stmts tab
    return ([Inst_Label str, Inst_R Push R6, Inst_RR Mov R6 R7, Inst_RI Sub R7 (Const spsub)]++ movs ++ code ++ [Inst_RR Mov R7 R6, Inst_R Pop R6, Inst Ret])
codegen_helper (List (x:xs)) tab = do
    res <- codegen_helper x tab
    code <- codegen_helper (List (xs)) tab
    return (res ++ code)
codegen_helper (List []) tab = return []
codegen_helper (Compound vs stmts) tab = do
    code <- codegen_helper stmts tab
    return code
codegen_helper (If cond tree) tab = do
    suffix <- labelSuffix
    condition <- codegen_helper cond tab
    block <- codegen_helper tree tab
    let prologue = [Inst_R Pop R0, Inst_RI Cmp R0 (Const 0), Inst_JmpI Jmp Eq (Label ("ifEnd" ++ suffix))]
    return (condition ++ prologue ++ block ++ [Inst_Label ("ifEnd" ++ suffix)])
codegen_helper (IfElse cond left right) tab = do
    suffix <- labelSuffix
    condition <- codegen_helper cond tab
    lft <- codegen_helper left tab
    rgt <- codegen_helper right tab
    let prologue = [Inst_R Pop R0, Inst_RI Cmp R0 (Const 0), Inst_JmpI Jmp Eq (Label ("ifElse" ++ suffix))]
    return (condition ++ prologue ++  lft ++ [Inst_JmpI Jmp Al (Label ("ifEnd" ++ suffix)),
        Inst_Label ("ifElse" ++ suffix)] ++ rgt ++ [Inst_Label ("ifEnd" ++ suffix)]) 
codegen_helper (While cond tree) tab = do
    suffix <- labelSuffix
    condition <- codegen_helper cond tab
    code <- codegen_helper tree tab
    return ((Inst_Label ("while_begin" ++ suffix):condition) ++ [Inst_R Pop R0, Inst_RI Cmp R0 (Const 0), Inst_JmpI Jmp Eq (Label ("while_end"++suffix) )] ++
        code ++ [Inst_JmpI Jmp Al (Label ("while_begin" ++ suffix)),Inst_Label ("while_end" ++ suffix)])
codegen_helper (Return tree) tab = do
    code <- codegen_helper tree tab
    return (code ++ [Inst_R Pop R0, Inst_RR Mov R7 R6, Inst_R Pop R6,Inst Ret])
codegen_helper (Deref tree) tab = do
    expr <- codegen_helper tree tab
    return (expr ++ [Inst_R Pop R0, Inst_Mem Ld R0 R0 Word,Inst_R Push R0])
codegen_helper (Addr (AnnotatedVar str t)) tab =
    let loc = M.lookup str tab
    in case loc of
        (Just (Local v)) ->
            return [Inst_RR Mov R0 R6,Inst_RI Sub R0 (Const v),Inst_R Push R0]
        (Just Global) ->
            return [Inst_RI Mov R0 (Label str), Inst_R Push R0]
        Nothing ->
            return (error $ "Undefined variable: " ++ str)
codegen_helper (FCall name pars ) tab = do
    parameters <- genPars pars 0 tab
    return (parameters ++ [Inst_JmpI Call Al (Label name)])
codegen_helper (FCallRet name pars ) tab = do
    parameters <- genPars pars 0 tab
    return (parameters ++ [Inst_JmpI Call Al (Label name),Inst_R Push R0])
codegen_helper (Var v) tab = error $ "Did not annotate var: " ++ v
codegen_helper (StrLabel str) tab = do
    (filename,count) <- get
    let label = str ++ "_" ++ filename
    return $ [Inst_RI Mov R0 (Label label),Inst_R Push R0]
codegen_helper x tab = trace ("Defaulting to empty on: " ++ (show x)) (return [])

genPars :: Tree -> Int -> SymTab -> State (String,Int) [Instruction]
genPars (List (x:xs)) parNum tab 
    | parNum < 4
    = do
        par <- codegen_helper x tab
        rest <- (genPars (List xs) (parNum+1) tab)
        return ( par ++ rest ++ [Inst_R Pop (intToReg parNum)])
    |otherwise = error $ "only 4 parameters supported"

genPars (List []) _ _ = return []

movPars :: Tree -> Int -> SymTab -> [Instruction]
movPars (List ((VarDec t str):rest)) x tab
    | x < 4 = 
        let (Just (Local loc)) = M.lookup str tab
        in 
            if t == V_Char then
                ((Inst_MemI St R6 (intToReg x) (Const (-loc)) Byte Displacement):(movPars (List rest) (x+1) tab))
            else
                ((Inst_MemI St R6 (intToReg x) (Const (-loc)) Word Displacement):(movPars (List rest) (x+1) tab))
    | otherwise = error "More than 4 parameters not supported yet"
movPars (List []) _ _ = []
assignLocal :: String -> Int -> State SymTab Int
assignLocal x size = do
    tab <- get
    let counter = M.lookup " count" tab
    case counter of
        (Just (Local v)) -> do
            let tab' = M.insert " count" (Local (v+size)) tab
            let tab'' = M.insert x (Local (v+size)) tab'
            put tab''
            return (v+size)
        Nothing -> do
            let tab' = M.insert " count" (Local size) tab
            let tab'' = M.insert x (Local size) tab'
            put tab''
            return size
            
{-
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
            |   While Tree Tree
-}
getLocals :: Tree -> SymTab
getLocals tree =
    snd $ runState (localTable tree) M.empty

localTable :: Tree -> State SymTab ()
localTable (VarDec t str) =
    do
        assignLocal str 2
        return ()
localTable (ArrayDec t str size) =
    do
        if t==V_Char then
            assignLocal str (size)
        else
            assignLocal str (size*2)
        return ()
localTable (FuncDec t str vars tree) = do
    localTable vars
    localTable tree
    return ()
localTable (List (x:xs)) = do
    localTable x
    localTable (List xs)
    return ()
localTable (If cond tree) = do
    localTable cond
    localTable tree
    return ()
localTable (IfElse cond left right) = do
    localTable cond
    localTable left
    localTable right
    return ()
localTable (Compound vars tree) = do
    localTable vars
    return ()
localTable (While cond tree) = do
    localTable cond
    localTable tree
    return ()
localTable _ = return ()
assemble_strings :: [(String,String)] -> String -> String
assemble_strings (s:strs) filename =
    let (label,string) = s
        label' = label ++ "_" ++ filename
        assembledString = label' ++ ":\n    "++".asciz \"" ++ string ++"\"\n"
    in assembledString ++ (assemble_strings strs filename)
assemble_strings [] _ = []

getType :: Tree -> Type
getType (AnnotatedVar str t) = t

getType (Num x) = V_Int

getType (Operator op left right) =
    let t1 = getType left
        t2 = getType right
    in max t1 t2
