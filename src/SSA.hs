module SSA where
import qualified Data.Map as M
import qualified Parse
import BlockDef
import Instructions(Register(..))
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

newSVar :: String -> SSA_Attr -> SSAState (SSAVar)
newSVar str attr = do
    tab <- get
    let val = M.lookup str tab
    case val of
        (Just x) -> do
            put $ M.insert str (x+1) tab    
            return (SSAVar str x attr)
        (Nothing) -> do
            put $ M.insert str 1 tab
            return (SSAVar str 0 attr)

getVar :: SSAAssignment -> SSAVar
getVar (AssignOp var _ _ _) = var
getVar (AssignVal var _) = var

toSSA :: [Parse.Tree] -> SSAState [SSAAssignment]
toSSA ((Parse.Num x):tree) = do
    v <- newSVar "_tmp" None
    rest <- toSSA tree
    return ((AssignVal v (Num x)):rest)

toSSA ((Parse.Operator op left right):tree) = do
    lft <- toSSA [left]
    rgt <- toSSA [right]
    let lvar = getVar $ last lft
    let rvar = getVar $ last rgt
    res <- newSVar "_tmp" None
    rest <- toSSA tree
    return (lft ++ rgt ++ ((AssignOp res op (Var lvar) (Var rvar)):(rest)))
toSSA ((Parse.Assign (Parse.VarAssign str) right):tree) = do
    rgt <- toSSA [right]
    res <- newSVar str None
    let rvar = getVar $ last rgt
    rest <- toSSA tree
    return (rgt ++ ((AssignVal res (Var rvar)):rest))
toSSA ((Parse.Var str):tree) = do
    v <- newSVar str None
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

test_s = [AssignVal (SSAVar "_tmp" 0 None) (Num 3),AssignVal (SSAVar "_tmp" 1 None) (Num 5),
    AssignOp (SSAVar "x" 0 None) Plus (Var (SSAVar "_tmp" 0 None)) (Var (SSAVar "_tmp" 1 None))]
    
test_a = fst $ runState (annotate test_s) M.empty
    
annotateOperand :: SSAVar -> State (M.Map (String,Int) Int) SSAVar
annotateOperand var@(SSAVar str n attr) = do
    tab <- get
    let res = M.lookup (str,n) tab
    case res of
        (Just v) ->
            return var
        (Nothing) -> do
            put $ M.insert (str,n) 1 tab
            return (SSAVar str n Kill)
annotate :: [SSAAssignment] -> State (M.Map (String,Int) Int) [SSAAssignment]
annotate ((AssignOp (SSAVar str n attr) op (Var a1) (Var a2)):rest) = do
    result <- annotate rest
    r2 <- annotateOperand a2
    r1 <- annotateOperand a1
    return $ (AssignOp (SSAVar str n Gen) op (Var r1) (Var r2)):result
annotate ((AssignVal (SSAVar str n attr) (Var var)):rest) = do
    result <- annotate rest
    v1 <- annotateOperand var
    return $ (AssignVal (SSAVar str n Gen) (Var v1)):rest
annotate ((AssignVal (SSAVar str n attr) (Num x)):rest) = do
    result <- annotate rest
    return $ (AssignVal (SSAVar str n Gen) (Num x)):result
annotate (a:rest) = do
    result <- annotate rest
    return $ a:result
annotate [] = return []

allocateVar :: SSAVar -> State ([Register],M.Map (String,Int) Register) SSAVar
allocateVar v@(SSAVar str n Gen) = do
    (regs,tab) <- get
    case regs of
        (r:rest) -> do
            put $ (rest,M.insert (str,n) r tab)
            return (Reg r)
        ([]) -> return v
allocateVar v@(SSAVar str n None) = do
    (regs,tab) <- get
    let res = M.lookup (str,n) tab
    case res of
        (Just reg) -> return (Reg reg)
        (Nothing) -> return v

allocateVar v@(SSAVar str n Kill) = do
    (regs,tab) <- get
    let res = M.lookup (str,n) tab
    case res of
        (Just reg) -> do
            let tab' = M.delete (str,n) tab
            put $ (reg:regs,tab')
            return $ Reg reg
        (Nothing) -> return v
allocate :: [SSAAssignment] -> State ([Register],M.Map (String,Int) Register) [SSAAssignment]

allocate ((AssignVal var (Num x)):rest) = do
    v1 <- allocateVar var
    result <- allocate rest
    return $ (AssignVal v1 (Num x)):result

allocate (a@(AssignVal var (Var v)):rest) = do
    v2 <- allocateVar v
    v1 <- allocateVar var
    
    result <- allocate rest
    return $ (AssignVal v1 (Var v2)):result
allocate ((AssignOp dest op (Var src1) (Var src2)):rest) = do
    s1 <- allocateVar src1
    s2 <- allocateVar src2
    d <- allocateVar dest
    result <- allocate rest
    return $ (AssignOp d op (Var s1) (Var s2)):result
allocate (x:rest) = do
    result <- allocate rest
    return $ x:result
allocate [] = return []




