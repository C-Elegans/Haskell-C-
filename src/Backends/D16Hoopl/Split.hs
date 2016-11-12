{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.Split where
import qualified Data.Map as Map
import Control.Monad
import Compiler.Hoopl
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Debug.Trace (trace)
import Prelude hiding ((<*>))


type SplitFact = Int
splitLattice :: DataflowLattice SplitFact
splitLattice = DataflowLattice {
    fact_name = "Temp counter",
    fact_bot = 0,
    fact_join = add }
    where
        add _ (OldFact old) (NewFact new) =
            let i = max old new 
                changeFlag = changeIf $ old /= new
            in (changeFlag, i)
            

countNodes :: FwdTransfer Node SplitFact
countNodes = mkFTransfer ft
  where
    ft :: Node e x -> SplitFact -> Fact x SplitFact
    ft (Label _)            f = f
    ft (Assign v e)         f = f + 1
    
    ft (Store _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond c tl fl)       f =
        mkFactBase splitLattice [(tl,f + (nodes c) ), (fl,f + (nodes c))]
    ft (Call vs _ _ bid)    f = 
        mapSingleton bid f
    ft (Return _)           _ = mapEmpty
        
    addVar f _ = f
    
nodes :: Expr -> Int
nodes (Binop o l r) = 1  + (nodes l)  + (nodes r)
nodes _ = 1;


splitExpr :: forall m . FuelMonad m => FwdRewrite m Node SplitFact
splitExpr = mkFRewrite split
  where
    split :: Node e x -> SplitFact -> m (Maybe (Graph Node e x))
    split (Assign v e) f = 
        let (lst,expr,i) = splitExpr e f
            graph = mkMiddles (lst ++ [(Assign v expr)])
        in  return $ Just graph
    split (Cond c l r) f = 
        let (lst,expr,i) = splitExpr c f
            graph = mkMiddles lst
        in  return $ Just $ graph <*> (mkLast (Cond expr l r))
    split n f = 
        return $ liftM insnToG $ Nothing
    
    splitExpr :: Expr -> Int -> ([Node O O],Expr,Int)
    splitExpr (Binop op l r) i = 
        let (l_nodes,l_e,l_i) = splitExpr l i
            (r_nodes,r_e,r_i) = splitExpr r l_i
            tmp = (Svar "tmp" (r_i+1) S_None)
            node = (Assign (S tmp) (Binop op l_e r_e ))
            in (l_nodes ++ r_nodes ++ [node], (SVar tmp), r_i+1)
    splitExpr (Unop op e) i =
        let (e_nodes,e_e,e_i) = splitExpr e i
            tmp = (Svar "tmp" (e_i+1) S_None)
            node = (Assign (S tmp) (Unop op e_e ))
            in (e_nodes ++ [node], (SVar tmp), e_i+1)
        
        
    splitExpr e i =
        ([],e,i)
    
