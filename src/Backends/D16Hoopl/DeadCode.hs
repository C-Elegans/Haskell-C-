{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Backends.D16Hoopl.DeadCode where
import Data.Maybe

import Compiler.Hoopl
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.OptSupport
import qualified Data.Set as S
{-
 -performs dead code elimination, requires a backward pass for determining liveness analysis
 -Does not remove expressions involving calls, however it does remove (None var) expressions
 -}
--Lattice
type Live = S.Set SVar
liveLattice :: DataflowLattice Live
liveLattice = DataflowLattice {
    fact_name = "Live SVariables",
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
    middleLive n@(Assign (S x) _)   f = addUses (S.delete x f) n
    middleLive (Assign (V x) _)   _ = error $ "Variable " ++ x ++ " is not in SSA Form"
    middleLive n@(Assign (R _) _)   f = addUses f n
    middleLive (Assign _ _)       _ = error $ "Broken assignment"
    middleLive n@(Store _ _ _)    f = addUses f n
    middleLive n@(None _)       f = addUses f n
    
    lastLive :: Node O C -> FactBase Live -> Live
    lastLive n@(Branch l)       f = addUses (fact f l) n
    lastLive n@(Cond _ tl fl)   f = addUses (fact f tl `S.union` fact f fl) n
    lastLive n@(Return _)       _ = addUses (fact_bot liveLattice) n
    
    fact :: FactBase (S.Set SVar) -> Label -> Live
    fact f l = fromMaybe S.empty $ lookupFact l f
    
    addUses :: S.Set SVar -> Node e x -> Live
    addUses = fold_EN (fold_EE addSVar)
    addSVar s (SVar v) = S.insert v s
    addSVar s _       = s
deadCode :: forall m. FuelMonad m => BwdRewrite m Node Live
deadCode = mkBRewrite del
  where
    del :: Node e x -> Fact x Live -> m (Maybe (Graph Node e x))
    del (Assign _ (Alloca i)) _ = return $ Just $ insnToG $ None (Alloca i)
    del (Assign (S x) e) live 
        | (mapEE containsCall e) == Nothing =
        case S.member x live of
            True -> return Nothing
            False -> return $ Just emptyGraph
    del (Assign (S _) e) _ 
        | (mapEE containsCall e) /= Nothing =
        return Nothing
        
    del (None (Call _ _)) _ = return $ Nothing
    del (None _) _ = return $ Just emptyGraph
    del _ _ = return $ Nothing
    
    containsCall c@(Call _ _) = Just c
    containsCall _ = Nothing
