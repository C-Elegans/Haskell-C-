{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.SSA where
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Compiler.Hoopl
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport

{-
 - This module converts all variables to SVars, variables with included numbers
 - Originally it was intended to do SSA conversion, but Hoopl cannot actually do SSA
 - conversion and it would not be useful to do so in light of fixpoints.
 -}
type SSAFact = Map.Map Var Int
ssaLattice :: DataflowLattice SSAFact
ssaLattice = DataflowLattice {
    fact_name = "SSA Map",
    fact_bot = Map.empty,
    fact_join = add }
    where
        add _ (OldFact old) (NewFact new) =
            let map = Map.unionWith max old new 
                changeFlag = changeIf  False
            in (changeFlag, map)
            
initSSAFact :: [Var] -> SSAFact
initSSAFact vars = Map.fromList  [(v,0) | v <- vars]

assignSSAVar :: FwdTransfer Node SSAFact
assignSSAVar = mkFTransfer ft
  where
    ft :: Node e x -> SSAFact -> Fact x SSAFact
    ft (Label _)            f = f
    ft (Assign _ _)         f = f
    
    ft (Store _ _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase ssaLattice [(tl,f), (fl,f)]
    ft (None _)             f = f
    ft (Return _)           _ = mapEmpty
    



ssaRewrite :: forall m . FuelMonad m => FwdRewrite m Node SSAFact
ssaRewrite = mkFRewrite ssa
  where
    ssa :: Node e x -> SSAFact -> m (Maybe (Graph Node e x))
    ssa node@(Assign _ _) f = 
        return $ fmap insnToG $ convertAssign f node
    ssa node f =
        return $ liftM insnToG $ mapVN (lookup f) node
    mapVN :: (Var -> Maybe Expr) -> MaybeChange (Node e x)
    mapVN = mapEN . mapEE . mapVE
    
    
    lookup :: SSAFact -> Var -> Maybe Expr
    lookup _ x = Just $ SVar $ Svar x 0 S_None              
    lookupAssign :: SSAFact -> Var -> Maybe Expr
    lookupAssign _ x =  Just $ SVar $ Svar x 0 S_None
    convertAssign :: SSAFact -> Node O O -> Maybe (Node O O)
    convertAssign f (Assign (V v) e) = do
        (SVar sv) <- lookupAssign f v
        
        let e'= (mapEE . mapVE) (lookup f) e
        let efinal = fromMaybe e e'
        return (Assign (S sv) efinal)
    convertAssign _ _ = Nothing
