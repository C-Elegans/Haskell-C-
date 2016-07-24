module TempCodegen where
import Instructions
import Parse
import Control.Monad.State
import qualified Data.Map as M
type SymTab = M.Map String Int
codegen :: Tree -> SymTab -> [Instruction]
codegen (Num x) _ =
    [Inst_I Push (Const x)]

codegen (Operator op left right) t =
    let lft = codegen left t
        rgt = codegen right t
        operation = case op of
            Plus ->
                [Inst_RR Add R0 R1]
            Minus ->
                [Inst_RR Sub R0 R1]
            Mul -> error "Mul unsupported"
            Div -> error "Div unsupported"
            
    in lft ++ rgt ++ [Inst_R Pop R1, Inst_R Pop R0] ++ operation ++ [Inst_R Push R0]
codegen (Var str) tab =
    let loc = M.lookup str tab
    in case loc of
        (Just v) ->
            [Inst_MemI Ld R0 R6 (Const (toInteger v)) Word Displacement, Inst_R Push R0]
        Nothing ->
            error $ "Undefined variable: " ++ str
codegen (Assign (VarAssign str) expr) tab =
    let vloc = M.lookup str tab
        code = codegen expr tab
    in case vloc of
        (Just loc) ->
            code ++ [Inst_R Pop R0, Inst_MemI St R6 R0 (Const (toInteger loc)) Word Displacement]
        (Nothing) -> error $ "Undefined variable: " ++ str

assignLocal :: String -> State SymTab Int
assignLocal x = do
    tab <- get
    let counter = M.lookup " count" tab
    case counter of
        (Just v) -> do
            let tab' = M.insert " count" (v+2) tab
            let tab'' = M.insert x (v+2) tab'
            put tab''
            return (v+2)
        Nothing -> do
            let tab' = M.insert " count" 2 tab
            let tab'' = M.insert x 2 tab'
            put tab''
            return 2
            
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
        assignLocal str
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
