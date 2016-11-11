{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.ConstProp where
import qualified Data.Map as Map
import Control.Monad
import Compiler.Hoopl
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport

type ConstFact = Map.Map SVar (WithTop Lit)
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
initFact vars = Map.fromList $ [(Svar v 0 S_None,Top) | v <- vars]

varHasLit :: FwdTransfer Node ConstFact
varHasLit = mkFTransfer ft
  where
    ft :: Node e x -> ConstFact -> Fact x ConstFact
    ft (Label _)            f = f
    ft (Assign (S x) (Lit k))   f = Map.insert x (PElem k) f
    ft (Assign (S x) _)         f = Map.insert x Top f
    ft (Store _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond (SVar x) tl fl) f =
        mkFactBase constLattice
            [(tl, Map.insert x (PElem (Bool True)) f),
             (fl, Map.insert x (PElem (Bool False))f)]
    ft (Cond _ tl fl)       f =
        mkFactBase constLattice [(tl,f), (fl,f)]
    ft (Call vs _ _ bid)    f = 
        mapSingleton bid (foldl toTop f vs)
            where toTop f (S v) = Map.insert v Top f
    ft (Return _)           _ = mapEmpty
    ft n _ = error $ "No ft defined for " ++ (show n)




constProp :: forall m . FuelMonad m => FwdRewrite m Node ConstFact
constProp = mkFRewrite cp
  where
    cp :: Node e x -> ConstFact -> m (Maybe (Graph Node e x))
    cp node f =
        return $ liftM insnToG $ mapVN (lookup f) node
    mapVN :: (SVar -> Maybe Expr) -> MaybeChange (Node e x)
    mapVN = mapEN . mapEE . mapSVE
    
    lookup :: ConstFact -> SVar -> Maybe Expr
    lookup f x = case Map.lookup x f of
        Just (PElem v) -> Just $ Lit v
        _              -> Nothing
