{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.Split where
import Control.Monad
import Control.Applicative hiding ((<*>))
import qualified Control.Applicative as A
import Compiler.Hoopl
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Prelude hiding ((<*>))
import Debug.Trace (trace)

{-
 -This pass splits expressions into assignments with a single binary, unary, or call expression.
 -This is necessary because we need to allocate registers for subexpressions and for instruction 
 -selection (as each assignment corresponds to 1 or 2 instructions for instruction selection. 
 -}

type SplitFact = Int
splitLattice :: DataflowLattice SplitFact
splitLattice = DataflowLattice {
    fact_name = "Temp counter",
    fact_bot = 0,
    fact_join = add }
    where
        add _ (OldFact old) (NewFact new) =
            let i = max old new 
                changeFlag = changeIf $ i /=old && i/= new
            in (changeFlag, i)
            

countNodes :: FwdTransfer Node SplitFact
countNodes = mkFTransfer ft
  where
    ft :: Node e x -> SplitFact -> Fact x SplitFact
    ft (Label _)            f = f
    ft (Assign _ _)         f = f + 1
    
    ft (Store _ _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase splitLattice [(tl,f + 1 ), (fl,f + 1)]
    ft (None _)             f = f
    ft (Return _)           _ = mapEmpty
        
    

newtype SplitE n i a = Split ([n] -> i -> ([n],a,i))
instance Monad (SplitE n i) where
    return a = Split $ \n i -> (n,a,i)
    (Split ma) >>= k = Split $ \n i ->
        let (n', a, i') = ma n i
            (Split ma') = k a
        in ma' (n++n') (i')
instance Applicative (SplitE n i) where
    pure = return
    (<*>) = ap
instance Functor (SplitE n i) where
    fmap = liftM
getTmp :: SplitE n Int SVar
getTmp = Split $ \n i ->
    let tmp = Svar "tmp" (i+1) S_None
    in (n,tmp,i+1)
put :: [Node O O] -> SplitE (Node O O) i ()
put nodes = Split $ \_ i -> (nodes,(),i)

runSplit :: SplitE n i a -> [n] -> i -> ([n],a,i)
runSplit (Split sp) = sp
splitExpr :: forall m . FuelMonad m => FwdRewrite m Node SplitFact
splitExpr = mkFRewrite split
  where
    split :: Node e x -> SplitFact -> m (Maybe (Graph Node e x))
    split (Assign v (Binop Div l r)) f =
        let (lst,left,i) = runSplitExpr l f
            (lst',right,_) = runSplitExpr r i
            graph = mkMiddles (lst ++ lst' ++ [(Assign v (Call "div" [left,right]))])
        in return $ Just graph
    split (Assign v (Binop Mul l r)) f =
        let (lst,left,i) = runSplitExpr l f
            (lst',right,_) = runSplitExpr r i
            graph = mkMiddles (lst ++ lst' ++ [(Assign v (Call "mul" [left,right]))])
        in return $ Just graph
    split (Assign v (Binop op l r)) f =
        let (lst,left,i) = runSplitExpr l f
            (lst',right,_) = runSplitExpr r i
            graph = mkMiddles (lst ++ lst' ++ [(Assign v (Binop op left right))])
        in return $ Just graph
    split (Assign v e) f = 
        let (lst,expr,_) = runSplitExpr e f
            graph = mkMiddles (lst ++ [(Assign v expr)])
        in  return $ Just graph
    split (Cond c l r) f = 
        let (lst,expr,_) = runSplitExpr c f
            graph = mkMiddles lst
        in  return $ Just $ graph <*> (mkLast (Cond expr l r))
--    split (None c@(Call name es)) f = trace ("Split call " ++ (show c)) $ 
--        let (lst,exprs,_) = fold_split es f
--            graph = mkMiddles lst
--        in return $ Just $ graph <*> ( mkMiddle $ None $ Call name exprs)
    split (None e) f=
        let (lst,expr,_) = runSplitExpr e f
            graph = mkMiddles lst
        in return $ Just $ graph <*> (mkMiddle (None expr))
    {-split (Store loc l@(Lit _) fl) f =-}
        {-let tmp = (Svar "tmp" (f+1) S_None)-}
            {-node = Assign (S tmp) l-}
        {-in return $ Just $ mkMiddles [node,Store loc (SVar tmp) fl]-}
    split n@(Store loc@(Binop Add _ (Lit _)) expr fl) f = trace ("Splitting node " ++ show n) $
        let (lst,expr',_) = runSplitExpr expr f
            tmp = Svar "tmp" (f+1) S_None
            graph = mkMiddles lst
        in return $ Just $ graph <*> (mkMiddles [Assign (S tmp) expr', Store loc expr' fl])
    split n@(Store loc@(Binop Sub r (Lit (Int i))) expr fl) f = trace ("Splitting node " ++ show n) $
        let (lst,expr',_) = runSplitExpr expr f
            tmp = Svar "tmp" (f+1) S_None
            graph = mkMiddles lst
        in return $ Just $ graph <*> (mkMiddles [Assign (S tmp) expr', Store
                                                  (Binop Add r (Lit (Int (0-i)))) expr' fl])
    split n@(Store loc expr fl) f = trace ("Splitting node " ++ show n) $
        let (lst,expr',i) = runSplitExpr expr f
            (lst',loc',i') = runSplitExpr loc i
            tmp = Svar "tmp" (i'+1) S_None
            tmp2 = Svar "tmp" (i'+2) S_None
            graph = mkMiddles $ lst ++ lst'
        in return $ Just $ graph <*> (mkMiddles [Assign (S tmp) expr', Store loc' expr' fl])
    split (Return e) f = 
        let (lst,expr,_) = runSplit (fold_split e) [] f
            graph = mkMiddles (lst)
        in  return $ Just $ graph <*> (mkLast (Return expr))
        
    split _ _ = 
        return $ liftM insnToG $ Nothing
   

    splitExpr :: Expr -> SplitE (Node O O) Int Expr
    {-splitExpr node _ | trace ("SplitExpr " ++ show node) False = undefined-}
    splitExpr l@(Lit (Int _)) = do
        tmp <- getTmp
        put [Assign (S tmp) l]
        return $ SVar tmp
    splitExpr l@(Load (Binop Sub r (Lit (Int i))) s) = do
        tmp <- getTmp
        put [Assign (S tmp) (Load (Binop Add r (Lit (Int (0-i))) )s)]
        return $ SVar tmp
    splitExpr l@(Load _ _) = do
        tmp <- getTmp
        put [Assign (S tmp) l]
        return $ SVar tmp
    splitExpr (Binop Mul l r) = do
        left <- splitExpr l
        right <- splitExpr r
        tmp <- getTmp
        put [Assign (S tmp) (Call "mul" [left,right])]
        return $ SVar tmp
    splitExpr (Binop Div l r) = do
        left <- splitExpr l
        right <- splitExpr r
        tmp <- getTmp
        put [Assign (S tmp) (Call "div" [left,right])]
        return $ SVar tmp
    splitExpr (Binop Mod l r) = do
        left <- splitExpr l
        right <- splitExpr r
        tmp <- getTmp
        put [Assign (S tmp) (Call "mod" [left,right])]
        return $ SVar tmp
    splitExpr (Binop op l r) = do
        left <- splitExpr l
        right <- splitExpr r
        tmp <- getTmp
        put [Assign (S tmp) (Binop op left right)]
        return $ SVar tmp
    splitExpr (Unop op e) = do
        exp <- splitExpr e
        tmp <- getTmp
        put [Assign (S tmp) (Unop op exp)]
        return $ SVar tmp
    splitExpr (Call n es) = do
        exprs <- fold_split es
        tmp <- getTmp
        put [Assign (S tmp) (Call n exprs)]
        return $ SVar tmp
    splitExpr (PostAssign (SVar a) e) = do
        tmp <- getTmp
        put [Assign (S tmp) (SVar a), Assign (S a) e]
        return $ SVar tmp
    splitExpr (PreAssign (SVar a) e) = do
        put [Assign (S a) e]
        return $ SVar a
    splitExpr e = --trace ("Default splitExpr on " ++ (show e)) $
        return e
    fold_split :: [Expr] -> SplitE (Node O O) Int [Expr]
    fold_split (e:es) = do
        exprs <- fold_split es
        expr <- splitExpr e
        return $ expr:exprs
    fold_split [] = return []
    runSplitExpr :: Expr -> Int -> ([Node O O],Expr,Int)
    runSplitExpr ex i = runSplit (splitExpr ex) [] i
