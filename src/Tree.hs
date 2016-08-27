module Tree where
import Parse (Tree(..), OP(..))
import Eval (funcop)
import Type
import TempCodegen (getType)
import Debug.Trace (trace)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import Data.Bits
import qualified Data.Map as M

m_apply :: (Tree -> (State a) Tree) -> Tree -> Bool -> (State a) Tree

m_apply f (Operator op left right) b = do
    l <- m_apply f left b
    r <- m_apply f right b
    f (Operator op l r)
m_apply f (UnaryOp op tree) b = do
    t <- m_apply f tree b
    f (UnaryOp op t)
m_apply f (Assign left right) b = do
    l <- m_apply f left b
    r <- m_apply f right b
    f (Assign l r)
m_apply f (List (x:xs)) b =
    do
        item <- m_apply f x b
        (List list') <- m_apply f (List xs) b
        f (List (item:list'))
m_apply f (List []) _ = f (List [])
m_apply f (Return tree) b = do
    tree' <- m_apply f tree b
    f (Return tree')
m_apply f (If cond tree) b =
    do
        cond' <- m_apply f cond b
        tree' <- m_apply f tree b
        f (If cond' tree')
m_apply f (IfElse cond left right) b = do
    c <- m_apply f cond b
    l <- m_apply f left b
    r <- m_apply f right b
    f (IfElse c l r)

m_apply f (Compound left right) b = do
    l <- m_apply f left b
    r <- m_apply f right b
    f (Compound l r)
m_apply f (FuncDef t str left right) clear =
    if clear then do
        state <- get
        l <- m_apply f left clear
        r <- m_apply f right clear
        put state
        f (FuncDef t str l r)
    else do
        l <- m_apply f left clear
        r <- m_apply f right clear
        f (FuncDef t str l r)
m_apply f (While cond tree) b = do
    c <- m_apply f cond b
    t <- m_apply f tree b
    f (While c t)
m_apply f (FCall str tree) b = do
    t <- m_apply f tree b
    f (FCall str t)
m_apply f (FCallRet str tree) b = do
    t <- m_apply f tree b
    f (FCallRet str t)
m_apply f (Deref tree) b = do
    tree' <- m_apply f tree b
    f (Deref tree')
m_apply f (Addr tree) b = do
    t <- m_apply f tree b
    f (Addr t)
m_apply f (Cast t expr) b = do
    e <- m_apply f expr b
    f (Cast t e)

m_apply f tree _ = f tree
getFunctions (List lst) = [FuncDef t str decls body | (FuncDef t str decls body) <- lst]
getGlobals (List lst) = [GlobalDec t str | (GlobalDec t str) <- lst]

type SymTab = M.Map String (Integer,Type)
type EV a = State SymTab a

lookUp :: String -> EV (Maybe (Integer,Type))
lookUp str = do
    symTab <- get
    return $ M.lookup str symTab

addSymbol :: String -> Integer -> Type -> EV ()
addSymbol str val t = do
    symTab <- get
    put $ M.insert str (val,t) symTab
    return ()

run_tree tree =
    let tree' = run_passes [(check_funcs,False)] $ run_passes passes tree
        (tree'',(ss,cnt)) = runState (m_apply getStrings tree' False) ([],0)
    in  (tree'',ss)

passes = [(check_defined,True),(type_check,True),(fix_ptr_arith,True),(const_subexpr_simplification,True), (arith_identity_removal,True), (mul_div_reduction, True)]
run_passes :: [(Tree -> State (M.Map k a) Tree, Bool)] -> Tree -> Tree
run_passes ((pass,flag):passes) tree =
    let (tree',symTab) = runState (m_apply pass tree flag ) (M.empty)

    in run_passes passes tree'
run_passes [] tree = tree

fix_ptr_arith all@(Operator op p1 p2)
    |((isPtr $getType p1) || (isArr $ getType p1)) && (op `elem` [Plus,Minus]) =
        return (Operator op p1 (Operator Shl p2 (Num 1)))

fix_ptr_arith x = return x

check_funcs :: Tree -> State (M.Map String (Type,[Type])) Tree
check_funcs func@(FuncDef t str (List pars) blk) = do
    let types = argTypes pars
    tab <- get
    put $ M.insert str (t,types) tab
    return func
check_funcs func@(FuncDec t str (List pars)) = do
    let types = argTypes pars
    tab <- get
    put $ M.insert str (t,types) tab
    return func
check_funcs call@(FCall str (List args)) = do
    tab <- get
    let ret = M.lookup str tab
    case ret of
        (Just (ret,expectedTypes)) -> do
            let actualTypes = map (getTypeFuncs tab) args
            let res = [canAssign t1 t2| (t1,t2) <- (zip expectedTypes actualTypes)]

            if and res && length res == length expectedTypes && length res == length actualTypes then
                return call
            else
                return $ error $ "Function " ++ str ++ " expects types " ++ (show expectedTypes) ++ " got " ++ (show actualTypes)
        Nothing -> return $ error $ "Function " ++ str ++ " not defined"
check_funcs tree = return tree
argTypes ((VarDec t str):rest) =
    t:(argTypes rest)
argTypes [] = []

getTypeFuncs tab (FCallRet str args) =
    let ret = M.lookup str tab
    in case ret of
        (Just (t,args)) -> t
        Nothing -> error $ "No definition found for function " ++ str
getTypeFuncs tab expr = getType expr
check_defined def@(GlobalDec t str) = do
    addSymbol str 2 t
    return def
check_defined (VarDec t v) = do
    addSymbol v 1 t
    return (VarDec t v)
check_defined tree@(ArrayDec t v sz) = do
    addSymbol v 1 (toArr t)
    return tree
check_defined (Var v) = do
    val <- lookUp v
    case val of
        Just (x,t) -> return (AnnotatedVar v t)
        Nothing -> error $ "Undefined Variable: " ++ v
check_defined (VarAssign v) = do
    val <- lookUp v
    case val of
        Just (x,t) -> return (AnnotatedVarAssign v t)
        Nothing -> error $ "Undefined Variable: " ++ v

check_defined tree = return tree

type_check expr@(Assign left right) = do
    let lType = getType left
    let rType = getType right
    if canAssign lType rType then
        return expr
    else
        return $ error $ "Cannot assign type " ++ (show rType) ++ " to " ++ (show lType) ++ "\n\t" ++ (show expr)
type_check tree = return tree
const_subexpr_simplification :: Tree -> EV Tree
const_subexpr_simplification (Operator op (Num l) (Num r)) =
    do
    return (Num (funcop op l r))
const_subexpr_simplification tree = return tree


arith_identity_removal :: Tree -> EV Tree
arith_identity_removal tree@(Operator op x (Num 0)) = do
    case op of
        Plus -> return x
        Minus -> return x
        Mul -> return (Num 0)
        _ -> return tree
arith_identity_removal tree@(Operator op (Num 0) x) = do
    case op of
        Plus -> return x
        Mul -> return (Num 0)
        _ -> return tree

arith_identity_removal (Operator Mul x (Num 1)) = return x
arith_identity_removal (Operator Mul (Num 1) x) = return x
arith_identity_removal x = return x

mul_div_reduction :: Tree -> EV Tree
mul_div_reduction (Operator Mul val (Num x))
    | isPowerOf2 x = do
        let shifter = countTrailingZeros x
        return (Operator Shl val (Num shifter))
mul_div_reduction (Operator Mul (Num x) val)
    | isPowerOf2 x = do
        let shifter = countTrailingZeros x
        return (Operator Shl val (Num shifter))
mul_div_reduction (Operator Div val (Num x))
    | isPowerOf2 x = do
        let shifter = countTrailingZeros x
        return (Operator Sar val (Num shifter))
mul_div_reduction x = return x

getStrings :: Tree -> State ([(String,String)],Int) Tree
getStrings (Str str) = do
    (lst,count) <- get
    let label = "str_const_" ++ (show count)
    put $ ((label,str):lst,count+1)
    return (StrLabel label)
getStrings x = return x
isPowerOf2 n = ((.&.) n (n-1)) == 0
