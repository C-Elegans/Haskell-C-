module Backends.D16Hoopl.ToIR where
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Compiler.Hoopl
import qualified Parse as P

buildExpr :: P.Tree -> Expr
buildExpr (P.Operator op left right) = 
    Binop (opToBinOp op) (buildExpr left) (buildExpr right)
buildExpr (P.Num n) =
    Lit (Int $ toInteger n)
buildExpr (P.AnnotatedVar name t) =
    Var name
    
buildNode :: P.Tree -> LabelMapM (Node O O)
buildNode (P.Assign (P.AnnotatedVarAssign nam t) (left)) =
    return $ Assign (nam) (buildExpr left)
buildNode x = error $ "No BuildNode defined for " ++ (show x) 


buildGraph :: P.Tree -> LabelMapM (Graph Node O O)
buildGraph (P.List (x:xs)) = do
    node <- buildGraph x
    graph <- buildGraph (P.List xs)
    return $ gSplice node graph
buildGraph (P.List []) =
    return $ gUnitOO $ BNil
buildGraph (P.Compound defs body) =
    buildGraph body
buildGraph (P.FCall name (P.List arglist)) = do
    lbl <- uniqueLabel
    let exprs = map buildExpr arglist
    let call = gUnitOC $ BlockOC BNil (Call [] name exprs lbl)
    let label = gUnitCO $ BlockCO (Label lbl) BNil
    return $ gSplice call label
buildGraph x =do
    node <- buildNode x 
    return $ gUnitOO $ BMiddle $ node
    
    

  
buildGraphCC :: P.Tree -> LabelMapM (Graph Node C C)
buildGraphCC (P.FuncDef t name args body) = do
    lbl <- labelFor name
    bodyGraph <- buildGraph body
    let graph = catNodeCOGraph (Label lbl) bodyGraph
    return $ catGraphNodeOC graph (Return [])


canBecomeGraph :: P.Tree -> Bool
canBecomeGraph (P.FuncDef _ _ _ _) = True
canBecomeGraph x = False

treeToIR (P.List lst) =
    map (\ x -> runSimpleUniqueMonad $ runWithFuel 0 ( run (buildGraphCC x))) (filter canBecomeGraph lst)
    
    


    

