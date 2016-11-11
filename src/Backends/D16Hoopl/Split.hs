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


type SSAFact = Map.Map Var Int
ssaLattice :: DataflowLattice SSAFact
ssaLattice = DataflowLattice {
    fact_name = "SSA Map",
    fact_bot = Map.empty,
    fact_join = add }
    where
        add _ (OldFact old) (NewFact new) =
            let map = Map.unionWith (max) old new 
                changeFlag = changeIf $ (Map.size map) > (Map.size old)
            in (changeFlag, map)
            
initSSAFact :: [Var] -> SSAFact
initSSAFact vars = Map.fromList $ [(v,0) | v <- vars]

assignSSAVar :: FwdTransfer Node SSAFact
assignSSAVar = mkFTransfer ft
  where
    ft :: Node e x -> SSAFact -> Fact x SSAFact
    ft (Label _)            f = f
    ft (Assign v _)         f = addVar f v
    
    ft (Store _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase ssaLattice [(tl,f), (fl,f)]
    ft (Call vs _ _ bid)    f = 
        mapSingleton bid (foldl addVar f vs)
    ft (Return _)           _ = mapEmpty
    addVar :: SSAFact  -> Assignable -> SSAFact
    addVar f (V v) = case Map.lookup v f of
        Just i  -> Map.insert v (i+1) f
        Nothing -> Map.insert v 0 f
    addVar f (S (Svar n i fl)) = 
        Map.insert n (i) f
        
    addVar f _ = f



ssaRewrite :: forall m . FuelMonad m => FwdRewrite m Node SSAFact
ssaRewrite = mkFRewrite ssa
  where
    ssa :: Node e x -> SSAFact -> m (Maybe (Graph Node e x))
    ssa node@(Assign _ _) f = 
        return $ liftM insnToG $ convertAssign f node
    ssa node f =
        return $ liftM insnToG $ mapVN (lookup f) node
    mapVN :: (Var -> Maybe Expr) -> MaybeChange (Node e x)
    mapVN = mapEN . mapEE . mapVE
    
    
    lookup :: SSAFact -> Var -> Maybe Expr
    lookup f x = case Map.lookup x f of
        Just i -> Just $ SVar $ Svar x i S_None
        _              -> Nothing
    lookupAssign :: SSAFact -> Var -> Maybe Expr
    lookupAssign f x = case Map.lookup x f of
        Just i -> Just $ SVar $ Svar x (i+1) S_None
        _              -> Just $ SVar $ Svar x 0 S_None
    convertAssign :: SSAFact -> Node O O -> Maybe (Node O O)
    convertAssign f (Assign (V v) e) = do
        (SVar sv) <- lookupAssign f v
        
        let e'= (mapEE . mapVE) (lookup f) e
        let efinal = case e' of
                        Just expr -> expr
                        Nothing -> e
        return (Assign (S sv) efinal)
