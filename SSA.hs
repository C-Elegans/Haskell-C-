module SSA where
import qualified Data.Map as M
import qualified Parse
import Parse (OP(..))
import Control.Monad
import Control.Monad.State

type SSATab = M.Map String (Int)
type SSAState a = State SSATab a
data SSAAssignment = AssignOp SSAVar OP Val Val |
                     AssignVal SSAVar Val
    
data SSAVar = SSAVar String Int 
data Val = Var SSAVar | Num Integer 
    deriving (Show)

instance Show SSAVar where
    show (SSAVar str x) = str ++ "_" ++ (show x)
instance Show SSAAssignment where
    show (AssignOp var op l r) =
        (show var) ++ " <- " ++ (show op) ++ " " ++ (show l) ++ " " ++ (show r)
    show (AssignVal var val) =
        (show var) ++ " <- " ++ (show val)
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
toSSA [] = return []
