module SSA where
import qualified Data.Map as M
import qualified Parse
import BlockDef
import Parse (OP(..),spaceTabs)
import Control.Monad
import Control.Monad.State
import Debug.Trace (trace)
type SSATab = M.Map String (Int)
type SSAState a = State SSATab a

print_SSAAssignmentList level (l:lst) = do
    spaceTabs level
    putStrLn (show l)
    print_SSAAssignmentList level lst
print_SSAAssignmentList _ [] = return ()

newSVar :: String -> SSAState (SSAVar)
newSVar str = do
    tab <- get
    let val = M.lookup str tab
    case val of
        (Just x) -> do
            put $ M.insert str (x+1) tab    
            return (SSAVar str x)
        (Nothing) -> do
            put $ M.insert str 1 tab
            return (SSAVar str 0)

getVar :: SSAAssignment -> SSAVar
getVar (AssignOp var _ _ _) = var
getVar (AssignVal var _) = var

toSSA :: [Parse.Tree] -> SSAState [SSAAssignment]
toSSA ((Parse.Num x):tree) = do
    v <- newSVar "_tmp"
    rest <- toSSA tree
    return ((AssignVal v (Num x)):rest)

toSSA ((Parse.Operator op left right):tree) = do
    lft <- toSSA [left]
    rgt <- toSSA [right]
    let lvar = getVar $ last lft
    let rvar = getVar $ last rgt
    res <- newSVar "_tmp"
    rest <- toSSA tree
    return (lft ++ rgt ++ ((AssignOp res op (Var lvar) (Var rvar)):(rest)))
toSSA ((Parse.Assign (Parse.VarAssign str) right):tree) = do
    rgt <- toSSA [right]
    res <- newSVar str
    let rvar = getVar $ last rgt
    rest <- toSSA tree
    return (rgt ++ ((AssignVal res (Var rvar)):rest))
toSSA ((Parse.Var str):tree) = do
    v <- newSVar str
    rest <- toSSA tree
    return ((AssignVal v (Var v)):rest)
toSSA ((Parse.Return expr):tree) = do
    val <- toSSA [expr]
    rest <- toSSA tree
    let retVar = getVar $ last val
    return (val ++ ((Ret (Var retVar)):rest))
toSSA (x:xs) =
    trace ("Error: " ++ show x) (
    do
        rest <- toSSA xs
        return (rest))
toSSA [] = return []
{-
        BFunc Type String [BVar] [Block]
    |   BStatement [Tree] 
    |   BIf Tree [Block]
    |   BIfElse Tree [Block] [Block]
    |   BVars [BVar]
    |   BSSA [SSAAssignment]
-}
runBlocks :: [Block] -> [Block]
runBlocks blocks =
    trace (show blocks) (
    fst $ runState (blk2SSA blocks) M.empty)
blk2SSA :: [Block] -> SSAState [Block]
blk2SSA ((BStatement trees):blocks) = 
    trace ("Statement") (
    do
        results <- toSSA trees
        rest <- blk2SSA blocks
        return ((BSSA results):rest))
blk2SSA ((BFunc t str vars blks):blocks) = 
    trace "Func" (
    do
        blks' <- blk2SSA blks
        rest <- blk2SSA blocks
        return ((BFunc t str vars blks'):rest))
blk2SSA ((BIf tree blks):blocks) = do
    blks' <- blk2SSA blks
    rest <- blk2SSA blocks
    return ((BIf tree blks'):rest)
blk2SSA ((BIfElse tree left right):blocks) = do
    lft <- blk2SSA left
    rgt <- blk2SSA right
    rest <- blk2SSA blocks
    return ((BIfElse tree lft rgt):rest)

blk2SSA ((BVars vars):blocks) = do
    rest <- blk2SSA blocks
    return ((BVars vars):rest)
blk2SSA (x:xs) = trace (show x) (do
    rest <- blk2SSA xs
    return (x:rest)
    )
blk2SSA [] = return []
