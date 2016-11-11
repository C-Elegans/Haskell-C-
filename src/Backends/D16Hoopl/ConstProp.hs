{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.ConstProp where
import qualified Data.Map as Map
import Control.Monad
import Compiler.Hoopl
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport

type ConstFact = Map.Map Var (WithTop Lit)
constLattice :: DataflowLattice ConstFact
constLattice = DataflowLattice {
    fact_name = "Constant Value",
    fact_bot = Map.empty,
    fact_join = joinMaps (extendJoinDomain constFactAdd) }
    where
        constFactAdd _ (OldFact old) (NewFact new) =
            if new == old then  (NoChange, PElem new)
            else                (SomeChange, Top)    
            
initFact :: [Var] -> ConstFact
initFact vars = Map.fromList $ [(v,Top) | v <- vars]

varHasLit :: FwdTransfer Node ConstFact
varHasLit = mkFTransfer ft
  where
    ft :: Node e x -> ConstFact -> Fact x ConstFact
    ft (Label _)            f = f
    ft (Assign x (Lit k))   f = Map.insert x (PElem k) f
    ft (Assign x _)         f = Map.insert x Top f
    ft (Store _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond (Var x) tl fl) f =
        mkFactBase constLattice
            [(tl, Map.insert x (PElem (Bool True)) f),
             (fl, Map.insert x (PElem (Bool False))f)]
    ft (Cond _ tl fl)       f =
        mkFactBase constLattice [(tl,f), (fl,f)]
    ft (Call vs _ _ bid)    f = 
        mapSingleton bid (foldl toTop f vs)
            where toTop f v = Map.insert v Top f
    ft (Return _)           _ = mapEmpty

constPropPass :: FuelMonad m => FwdPass m Node ConstFact
constPropPass = FwdPass
  { fp_lattice  = constLattice
  , fp_transfer = varHasLit
  , fp_rewrite  = constProp }


constProp :: forall m . FuelMonad m => FwdRewrite m Node ConstFact
constProp = mkFRewrite cp
  where
    cp :: Node e x -> ConstFact -> m (Maybe (Graph Node e x))
    cp node f =
        return $ liftM insnToG $ mapVN (lookup f) node
    mapVN :: (Var -> Maybe Expr) -> MaybeChange (Node e x)
    mapVN = mapEN . mapEE . mapVE
    
    lookup :: ConstFact -> Var -> Maybe Expr
    lookup f x = case Map.lookup x f of
        Just (PElem v) -> Just $ Lit v
        _              -> Nothing
