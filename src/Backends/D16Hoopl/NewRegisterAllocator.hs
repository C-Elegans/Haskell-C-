{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.NewRegisterAllocator where
import qualified Data.Map as Map
import qualified Data.Set as S
import Control.Monad
import Instructions (Register(..))
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

data RaFact = RaFact {
  sp :: Int,
  mapping :: Map.Map Var Register,
  regs :: S.Set Register
}
  deriving (Show, Eq)

raEmpty :: RaFact
raEmpty = RaFact {sp=0, mapping=Map.empty, regs=S.fromList [R0, R1, R2, R3, R4, R5]}
  
raLattice :: DataflowLattice RaFact
raLattice = DataflowLattice {
    fact_name = "Register Allocator Map",
    fact_bot = raEmpty,
    fact_join = add }
    where
        add _ (OldFact old) (NewFact new) =
            let map = Map.union (mapping old) (mapping new)
                s = max (sp old) (sp new)
                r = (regs old)
                f = RaFact {sp=s, mapping=map, regs=r}
                changeFlag = changeIf (f /= old || f /= new)
            in (changeFlag, f)
            

raTransfer :: FwdTransfer Node RaFact
raTransfer = mkFTransfer ft
  where
    ft :: Node e x -> RaFact -> Fact x RaFact
    ft (Label _)            f = f
    ft (Assign (S v) (Reg r))         f = f
    
    ft (Store _ _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase raLattice [(tl,f), (fl,f)]
    ft (None _)             f = f
    ft (Return _)           _ = mapEmpty
    



raRewrite :: forall m . FuelMonad m => FwdRewrite m Node RaFact
raRewrite = deepFwdRw ra
  where
    ra :: Node e x -> RaFact -> m (Maybe (Graph Node e x))
    ra (Assign (S sv) (SVar (Svar n i S_Kill))) f
      | isJust (nameToReg n) =
        
      return $ Just $ insnToG $ Assign (S sv) (Reg (fromJust (nameToReg n)))
    ra _ _ = return $ Nothing

    nameToReg " r0" = Just R0
    nameToReg " r1" = Just R1
    nameToReg " r2" = Just R2
    nameToReg " r3" = Just R3
    nameToReg " r4" = Just R4
    nameToReg " r5" = Just R5
    nameToReg " r6" = Just R6
    nameToReg " r7" = Just R7
    nameToReg _ = Nothing
