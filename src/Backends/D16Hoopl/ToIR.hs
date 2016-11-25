module Backends.D16Hoopl.ToIR where
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Compiler.Hoopl
import Instructions (Register(..))
import qualified Parse as P
import Prelude hiding ((<*>))
import Debug.Trace(trace)

buildExpr :: P.Tree -> Expr
{-buildExpr tr | trace ("BuildExpr " ++ (show tr)) False = undefined-}
buildExpr (P.Operator op left right) = 
    Binop (opToBinOp op) (buildExpr left) (buildExpr right)
buildExpr (P.Num n) =
    Lit (Int $ n)
buildExpr (P.Addr (P.AnnotatedVar name t)) = 
    Unop Addr (Var name)
buildExpr (P.AnnotatedVar name t) =
    Var name
buildExpr (P.UnaryOp op t) =
    Unop (opToUnOp op) (buildExpr t)
buildExpr (P.Deref x) = 
    Load (buildExpr x)
buildExpr (P.StrLabel str) =
    Str str
buildExpr (P.FCallRet name (P.List exprs)) =
    Call name (map buildExpr exprs)
buildExpr (P.AnnotatedFCallRet name (P.List exprs) t) =
    Call name (map buildExpr exprs)
buildExpr t = error $ "No BuildExpr defined for " ++ (show t)
    
buildNode :: P.Tree -> LabelMapM (Node O O)
{-buildNode tr | trace ("Buildnode " ++ (show tr)) False = undefined-}
buildNode (P.Assign (P.AnnotatedVarAssign nam t) (left)) =
    return $ Assign (V nam) (buildExpr left)
buildNode (P.Assign (P.Deref expr) (left)) =
    return $ Store (buildExpr expr) (buildExpr left)
buildNode x = error $ "No BuildNode defined for " ++ (show x) 


buildGraph :: P.Tree -> LabelMapM (Graph Node O O)
buildGraph (P.List (x:xs)) = do
    node <- buildGraph x
    graph <- buildGraph (P.List xs)
    return $ node <*> graph
buildGraph (P.List []) =
    return $ emptyGraph
buildGraph (P.Compound defs body) =
    buildGraph body
buildGraph (P.FCall name (P.List arglist)) = do
    let exprs = map buildExpr arglist
    let call = mkMiddle (None (Call name exprs))
    
    return $ call
buildGraph (P.Assign (P.AnnotatedVarAssign nam t) (P.AnnotatedFCallRet name (P.List arglist) t')) = do
    let exprs = map buildExpr arglist
    let call = mkMiddle (Assign (V nam) (Call name exprs))
    
    return $ call 
buildGraph (P.If con block) = do
    lblIf <- uniqueLabel
    lblNext <- uniqueLabel
    blockGraph <- buildGraph block
    let blockGraph' = (mkFirst (Label lblIf)) <*> blockGraph <*> (mkLast (Branch lblNext))
    let cond = mkLast (Cond (buildExpr con) lblIf lblNext)
    return $ cond |*><*| blockGraph' |*><*| ( mkFirst (Label lblNext) )

buildGraph (P.IfElse cond i e) = do
    lblIf <- uniqueLabel
    lblElse <- uniqueLabel
    lblNext <- uniqueLabel
    ifGraph <- buildGraph i 
    elseGraph <- buildGraph e
    let ifGraph' = (mkFirst (Label lblIf)) <*> ifGraph <*> (mkLast (Branch lblNext))
    let elseGraph' = (mkFirst (Label lblElse)) <*> elseGraph <*> (mkLast (Branch lblNext))
    let condGraph = mkLast (Cond (buildExpr cond) lblIf lblElse)
    return $ condGraph |*><*| ifGraph' |*><*| elseGraph' |*><*| (mkFirst (Label lblNext) )
buildGraph (P.Return P.EmptyTree) = do
    let ret = mkLast (Return [])
    dummyLabel <- uniqueLabel
    let dummy = mkFirst (Label dummyLabel)
    return $ ret |*><*| dummy
buildGraph (P.Return expr) = do
    let ret = mkLast (Return [(buildExpr expr)])
    dummyLabel <- uniqueLabel
    let dummy = mkFirst (Label dummyLabel)
    return $ ret |*><*| dummy
buildGraph (P.While cond tree) = do
    lblCond <- uniqueLabel
    lblWhile <- uniqueLabel
    whileBody <- (buildGraph tree)
    lblNext <- uniqueLabel
    
    let condGraph = 
            (mkLast (Branch lblCond)) |*><*|
            (mkFirst (Label lblCond)) <*>
            (mkLast (Cond (buildExpr cond) lblWhile lblNext))
    let whileGraph =    
            (mkFirst (Label lblWhile))  <*> 
            (whileBody)                 <*>
            (mkLast (Branch lblCond))   |*><*|
            (mkFirst (Label lblNext))
    return $ condGraph |*><*| whileGraph
    
buildGraph x =do
    node <- buildNode x 
    return $ mkMiddle node
    
    

args2Vars :: [P.Tree] -> [Var]
args2Vars ((P.VarDec t str):xs) = str:(args2Vars xs)
args2Vars [] = []

assignVars :: [P.Tree] -> [Register] -> [Node O O]
assignVars ((P.VarDec t str):xs) (r:rs) = (Assign (V str) (Reg r)):(assignVars xs rs)
assignVars [] _ = []
assignVars x [] = error $ "Cannot assign register to " ++ (show x)


buildGraphCC :: P.Tree -> LabelMapM (Proc)
buildGraphCC (P.FuncDef t name (P.List args) body) = do
    lbl <- uniqueLabel
    bodyGraph <- buildGraph body
    let pars = assignVars args [R0, R1, R2, R3]
    let graph = (mkMiddles pars) <*> bodyGraph
    let graph' = catNodeCOGraph (Label lbl) graph
    let graph'' = catGraphNodeOC graph' (Return [])
    return $ Proc{ name=name, args=(args2Vars args), entry = lbl, body = graph''}


canBecomeGraph :: P.Tree -> Bool
canBecomeGraph (P.FuncDef _ _ _ _) = True
canBecomeGraph x = False

treeToIR :: P.Tree -> [Proc]
treeToIR (P.List lst) =
    map (\ x -> runSimpleUniqueMonad $ runWithFuel 0 ( run (buildGraphCC x))) (filter canBecomeGraph lst)
    
    


    

