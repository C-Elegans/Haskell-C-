{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.RegisterAllocator where
import qualified Data.Map as Map
import Control.Monad
import Compiler.Hoopl
import Instructions (Register(..))
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Debug.Trace(trace)

type RegFact = (Map.Map SVar Register, [Register])
regLattice :: DataflowLattice RegFact
regLattice = DataflowLattice {
    fact_name = "Registers",
    fact_bot = (Map.empty,[R0,R1,R2,R3]),
    fact_join = join }
    where
        join _ (OldFact (oldM,oldR)) (NewFact (newM,newR)) = (ch, j)
            where
              j = (newM `Map.union` oldM, if (length oldR) < (length newR) then oldR else newR)
              ch = changeIf $ Map.size (fst j) == Map.size oldM
     
freeRegs = [R0,R1,R2,R3]
            
initRegs :: [Var] -> RegFact
initRegs vars = (Map.fromList $ zip svars $take (length vars) freeRegs , drop (length vars) freeRegs)
    where 
        svars = [Svar v 0 S_None | v <- vars]
assignRegister :: FwdTransfer Node RegFact
assignRegister = mkFTransfer ft
  where
    ft :: Node e x -> RegFact -> Fact x RegFact
    ft (Label _)            f = f
    ft n@(Assign (S x) _)     f = assignR (killVars f n) x
    ft (Assign (R r) _)     f = f
    ft (Store _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase regLattice [(tl,f), (fl,f)]
    ft (Call vs _ _ bid)    f = mapSingleton bid f
    ft (Return _)           _ = mapEmpty
    
    killVars :: RegFact -> Node e x -> RegFact
    killVars f = fold_EN (killVar) f
    
    killVar :: RegFact -> Expr -> RegFact
    killVar (m,r) (SVar v@(Svar _ _ S_Kill)) = case Map.lookup v m of
        Just reg -> (Map.delete v m, reg:r)
        _ -> (m,r)
    killVar f _ = f
    assignR :: RegFact -> SVar -> RegFact
    assignR (map,regs) v = 
        if length regs /= 0  then
            (Map.insert v (head regs) map, tail regs)
        else
            (map,regs)   
   
    fold_NE = fold_EN . fold_EE

rewriteRegister :: forall m . FuelMonad m => FwdRewrite m Node RegFact
rewriteRegister = mkFRewrite cp
  where
    cp :: Node e x -> RegFact -> m (Maybe (Graph Node e x))
    cp n@(Assign (S s) e) f = 
        return $ liftM insnToG $ assgn
        where 
            assgn =  node
            reg = (assignR f s)
            node = case reg of 
                Just a -> mapSVN (lookupE f) (Assign a e)
                Nothing -> mapSVN (lookupE f) n
    cp node f =
        return $ liftM insnToG $ mapSVN (lookupE f) node
    mapSVN :: (SVar -> Maybe Expr) -> MaybeChange (Node e x)
    mapSVN = mapEN . mapEE . mapSVE
    
    lookupA :: RegFact -> SVar -> Maybe Assignable
    lookupA f s = case lookup f s of
        Just reg -> Just $ R reg
        Nothing  -> Nothing
    lookupE :: RegFact -> SVar -> Maybe Expr
    lookupE f s = case lookup f s of
        Just reg -> Just $ Reg reg
        Nothing  -> Nothing
    lookup :: RegFact -> SVar -> Maybe Register
    lookup (f,rs) x = trace ((show f) ++ (show rs)) $
        case Map.lookup x f of
            Just reg       -> Just $ reg
            _              -> Nothing
    assignR :: RegFact -> SVar -> Maybe Assignable
    assignR (map,regs) v =
        case Map.lookup v map of
            Just r -> Just $ R r
            Nothing -> Nothing
