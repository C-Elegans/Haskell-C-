module Tree where
import Parse (Tree(..), OP(..))
import Eval (funcop)
import Type
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
m_apply f (FuncDec t str left right) clear =
    if clear then do
        state <- get
        l <- m_apply f left clear
        r <- m_apply f right clear
        put state
        f (FuncDec t str l r)
    else do
        l <- m_apply f left clear
        r <- m_apply f right clear
        f (FuncDec t str l r)
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
getFunctions (List lst) = [FuncDec t str decls body | (FuncDec t str decls body) <- lst]
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
    let tree' = run_passes passes tree
        (tree'',(ss,cnt)) = runState (m_apply getStrings tree' False) ([],0)
    in  (tree'',ss)
passes = [check_defined,const_subexpr_simplification, arith_identity_removal, mul_div_reduction]
run_passes (pass:passes) tree =
    let (tree',symTab) = runState (m_apply pass tree True ) (M.empty)
    
    in run_passes passes tree'
run_passes [] tree = tree


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


const_subexpr_simplification :: Tree -> EV Tree
const_subexpr_simplification (Operator op (Num l) (Num r)) =
    do 
    return (Num ((funcop op) l r))
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
        return (Operator Shr val (Num shifter))
mul_div_reduction x = return x

getStrings :: Tree -> State ([(String,String)],Int) Tree    
getStrings (Str str) = trace ("Found string: " ++ str) (do
    (lst,count) <- get
    let label = "str_const_" ++ (show count)
    put $ ((label,str):lst,count+1)
    return (StrLabel label))
getStrings x = return x
isPowerOf2 n = ((.&.) n (n-1)) == 0




