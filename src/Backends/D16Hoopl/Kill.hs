{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Backends.D16Hoopl.Kill where
import Data.Maybe

import Compiler.Hoopl
import Control.Monad
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.OptSupport
import qualified Data.Set as S

{-
 -Annotates variable lifetimes by setting the flags of an SVar to S_Kill upon the final usage of
 -that variable. Not used currently as it seems Linearscan does this on its own, but it may come 
 -in handy if I ever decide to write a register allocator (either graph coloring or linear)
 -}

--Lattice
type Kill = S.Set SVar
killLattice :: DataflowLattice Kill
killLattice = DataflowLattice {
    fact_name = "Kill SVariables",
    fact_bot = S.empty,
    fact_join = add 
    }
    where add _ (OldFact old) (NewFact new) = (ch, j)
            where
              j = new `S.union` old
              ch = changeIf (S.size j > S.size old )

killed :: BwdTransfer Node Kill
killed = mkBTransfer3 firstLive middleLive lastLive
  where
    firstLive :: Node C O -> Kill -> Kill
    firstLive (Label _) f = f
    
    middleLive :: Node O O -> Kill -> Kill
    middleLive n@(Assign (S x) _)   f = addUses (S.delete x f) n
    middleLive (Assign (V x) _)   _ = error $ "Variable " ++ x ++ " is not in SSA Form"
    middleLive (Assign (R _) _)   _ = error $ "SSA Pass not intended to be run on registers"
    middleLive n@(Store _ _)    f = addUses f n
    middleLive n@(None _)       f = addUses f n
    
    lastLive :: Node O C -> FactBase Kill -> Kill
    lastLive n@(Branch l)       f = addUses (fact f l) n
    lastLive n@(Cond _ tl fl)   f = addUses (fact f tl `S.union` fact f fl) n
    lastLive n@(Return _)       _ = addUses (fact_bot killLattice) n
    
    fact :: FactBase (S.Set SVar) -> Label -> Kill
    fact f l = fromMaybe S.empty $ lookupFact l f
    
    addUses :: S.Set SVar -> Node e x -> Kill
    addUses = fold_EN (fold_EE addSVar)
    addSVar s (SVar v) = S.insert v s
    addSVar s _       = s

killVars :: forall m. FuelMonad m => BwdRewrite m Node Kill
killVars = mkBRewrite kill
  where
    kill :: Node e x -> Fact x Kill -> m (Maybe (Graph Node e x))
       
    kill n f = return $ liftM insnToG $ kill_node f n
    mapSVN = (mapEN . mapEE . mapSVE) 
    kill_node :: Fact x Kill -> Node e x -> Maybe (Node e x)
    --Open Nodes
    kill_node f n@(Assign _ _) = mapSVN (kill_var f) n
    kill_node f n@(Store _ _) = mapSVN (kill_var f) n
    
    
    --Closed Nodes
    kill_node f n@(Cond _ tl fl) = mapSVN (kill_var (fact f tl `S.union` fact f fl)) n
    kill_node _ n@(Return _) = mapSVN (kill_var S.empty) n
    kill_node f n@(None _)   = mapSVN (kill_var f) n
    kill_node _ _ = Nothing
    
    kill_var :: Kill -> SVar -> Maybe Expr
    kill_var f v@(Svar n i _)= 
        if S.member v f then
            Just $ SVar $ Svar n i S_None
        else
            Just $ SVar $ Svar n i S_Kill

    fact :: FactBase (S.Set SVar) -> Label -> Kill
    fact f l = fromMaybe S.empty $ lookupFact l f
    
