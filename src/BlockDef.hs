module BlockDef where
import Parse(Tree,OP(..))
import Type
data BVar = BVar Type String
    deriving (Show)
data Block =
        BFunc Type String [BVar] [Block]
    |   BStatement [Tree] 
    |   BIf Tree [Block]
    |   BIfElse Tree [Block] [Block]
    |   BVars [BVar]
    |   BSSA [SSAAssignment]
    deriving (Show)
data SSAAssignment =    AssignOp SSAVar OP Val Val 
                     |  AssignVal SSAVar Val
                     |  Ret Val
    
data SSA_Attr = Gen | Kill | None 
instance Show SSA_Attr where
    show Gen = "Gen"
    show Kill = "Kill"
    show None = ""
data SSAVar = SSAVar String Int SSA_Attr
data Val = Var SSAVar | Num Int 
    deriving (Show)

instance Show SSAVar where
    show (SSAVar str x attr) = str ++ "_" ++ (show x) ++ " " ++ (show attr)
instance Show SSAAssignment where
    show (AssignOp var op l r) =
        (show var) ++ " <- " ++ (show op) ++ " " ++ (show l) ++ " " ++ (show r)
    show (AssignVal var val) =
        (show var) ++ " <- " ++ (show val)
    show (Ret val) =
        "ret " ++ (show val)
