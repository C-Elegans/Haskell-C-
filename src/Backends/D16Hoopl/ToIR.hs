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
buildExpr (P.UnaryOp op t) =
    Unop (opToUnOp op) (buildExpr t)
buildExpr t = error $ "No BuildExpr defined for " ++ (show t)
    
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
buildGraph (P.If con block) = do
    lblIf <- uniqueLabel
    lblNext <- uniqueLabel
    blockGraph <- buildGraph block
    let blockGraph' = catGraphNodeOC blockGraph (Branch lblNext)
    let blockGraph'' = catNodeCOGraph (Label lblIf) blockGraph'
    let cond = gUnitOC $ BlockOC BNil (Cond (buildExpr con) lblIf lblNext)
    return $ gSplice (gSplice cond blockGraph'') $ gUnitCO $ BlockCO (Label lblNext) BNil

buildGraph (P.IfElse cond i e) = do
    lblIf <- uniqueLabel
    lblElse <- uniqueLabel
    lblNext <- uniqueLabel
    ifGraph <- buildGraph i 
    elseGraph <- buildGraph e
    let ifGraph' = catNodeCOGraph (Label lblIf) (catGraphNodeOC ifGraph (Branch lblNext))
    let elseGraph' = catNodeCOGraph (Label lblElse) (catGraphNodeOC elseGraph (Branch lblNext))
    let condGraph = (blockGraph $ BlockOC BNil (Cond (buildExpr cond) lblIf lblElse))
    return $ gSplice condGraph $ gSplice ifGraph' (gSplice elseGraph' (blockGraph $ BlockCO (Label lblNext) BNil))
    
buildGraph (P.Return expr) = do
    let ret =blockGraph $ BlockOC BNil (Return [(buildExpr expr)])
    dummyLabel <- uniqueLabel
    let dummy = blockGraph $ BlockCO (Label dummyLabel) BNil
    return $ gSplice ret dummy
    
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
    
    


    

