{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Backends.D16Hoopl.DeadCode where
import Data.Maybe

import Compiler.Hoopl
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.OptSupport
import Debug.Trace (trace)
import qualified Data.Set as S

--Lattice
type Live = S.Set Var
liveLattice :: DataflowLattice Live
liveLattice = DataflowLattice {
    fact_name = "Live variables",
    fact_bot = S.empty,
    fact_join = add 
    }
    where add _ (OldFact old) (NewFact new) = (ch, j)
            where
              j = new `S.union` old
              ch = changeIf (S.size j > S.size old)

liveness :: BwdTransfer Node Live
liveness = mkBTransfer3 firstLive middleLive lastLive
  where
    firstLive :: Node C O -> Live -> Live
    firstLive (Label _) f = f
    
    middleLive :: Node O O -> Live -> Live
    middleLive n@(Assign x _)   f = addUses (S.delete x f) n
    middleLive n@(Store _ _)    f = addUses f n
    
    lastLive :: Node O C -> FactBase Live -> Live
    lastLive n@(Branch l)       f = addUses (fact f l) n
    lastLive n@(Cond _ tl fl)   f = addUses (fact f tl `S.union` fact f fl) n
    lastLive n@(Call vs _ _ l)  f = addUses (fact f l ` S.difference` S.fromList vs) n
    lastLive n@(Return _)       _ = addUses (fact_bot liveLattice) n
    
    fact :: FactBase (S.Set Var) -> Label -> Live
    fact f l = fromMaybe S.empty $ lookupFact l f
    
    addUses :: S.Set Var -> Node e x -> Live
    addUses = fold_EN (fold_EE addVar)
    addVar s (Var v) = S.insert v s
    addVar s _       = s
deadCode :: forall m. FuelMonad m => BwdRewrite m Node Live
deadCode = mkBRewrite del
  where
    del :: Node e x -> Fact x Live -> m (Maybe (Graph Node e x))
    del n@(Assign x _) live = 
        case S.member x live of
            True -> return Nothing
            False -> return $ trace ("Removing node (dead): " ++ (show n)) $ Just emptyGraph
        
    del _ _ = return $ Nothing
    
