module Tree where
import Parse (Tree(..), OP(..), Type(..))
import Eval (funcop)
import Debug.Trace (trace)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import qualified Data.Map as M

m_apply :: (Tree -> EV Tree) -> Tree -> EV Tree 

m_apply f (Operator op left right) = do
    l <- m_apply f left
    r <- m_apply f right
    f (Operator op l r)
m_apply f (Assign left right) = do
    l <- m_apply f left
    r <- m_apply f right
    f (Assign l r)
m_apply f (List (x:xs)) =
    do
        item <- m_apply f x
        (List list') <- m_apply f (List xs)
        f (List (item:list'))
m_apply f (List []) = f (List [])
m_apply f (Return tree) = do
    tree' <- m_apply f tree
    f (Return tree')
m_apply f (If cond tree) = 
    do
        cond' <- m_apply f cond
        tree' <- m_apply f tree
        f (If cond' tree')
m_apply f (IfElse cond left right) = do
    c <- m_apply f cond
    l <- m_apply f left
    r <- m_apply f right
    f (IfElse c l r)

m_apply f (Compound left right) = do
    l <- m_apply f left
    r <- m_apply f right
    f (Compound l r)
m_apply f (FuncDec t str left right) = do
    state <- get
    l <- m_apply f left
    r <- m_apply f right
    put state
    f (FuncDec t str l r)
m_apply f (FCall str tree) = do
    t <- m_apply f tree
    f (FCall str t)

m_apply f tree = f tree
getFunctions (List lst) = [FuncDec t str decls body | (FuncDec t str decls body) <- lst]
getGlobals (List lst) = [VarDec t str | (VarDec t str) <- lst]

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
    

passes = [check_defined,const_subexpr_simplification, arith_identity_removal]
run_passes (pass:passes) tree =
    let (tree',symTab) = runState (m_apply pass tree) (M.empty)
    
    in run_passes passes tree'
run_passes [] tree = tree


    
check_defined (VarDec t v) = do
    addSymbol v 1 t
    return (VarDec t v)
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





