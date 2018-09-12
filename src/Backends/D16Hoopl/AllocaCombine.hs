{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Backends.D16Hoopl.AllocaCombine where
import Data.Maybe

import Compiler.Hoopl
import Compiler.Hoopl.Internals (uniqueToLbl)
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.OptSupport
import Prelude hiding ((<*>))
import Debug.Trace (trace)


type AllocaFact = Int

allocaLattice :: DataflowLattice AllocaFact
allocaLattice = DataflowLattice {
  fact_name = "Total alloca",
  fact_bot = 0,
  fact_join = amax }
  where
    amax :: Label -> OldFact AllocaFact -> NewFact AllocaFact -> (ChangeFlag, AllocaFact)
    amax _ (OldFact old) (NewFact new) =
      let a = max old new
          cf = changeIf $ a /= old || a /= new
      in (cf, a)

combineRw :: forall m. FuelMonad m => BwdRewrite m Node AllocaFact
combineRw = mkBRewrite comb
  where
    comb :: Node e x -> Fact x AllocaFact -> m (Maybe (Graph Node e x))
    comb (Assign _ _) f | trace (show f) False = undefined
    comb (Assign _ (Alloca i)) f =
      return $ Just $ insnToG $ (Assign (V " ") (Alloca i))
    comb (Label l) f
      | (uniqueToLbl 1) == l =
        return $ Just $ (insnToG (Label l)) <*> (insnToG (None (Alloca f)))
    comb _ _ = return Nothing



combineTransfer = mkBTransfer3 fl ml ll
  where
    fl :: Node C O -> AllocaFact -> AllocaFact
    
    fl (Label _)            f = f

    ml :: Node O O -> AllocaFact -> AllocaFact
    ml (Assign _ (Alloca i))             f = f + i
    ml (Assign _ e)         f = f
    ml (Store _ _ _)        f = f
    ml (None _) f = f

    ll :: Node O C -> FactBase AllocaFact -> AllocaFact
    ll (Branch l)           f = fact f l
    ll (Cond _ tl fl)       f =
        max (fact f tl) (fact f fl)
    ll (Return _)           _ = fact_bot allocaLattice

    fact :: FactBase AllocaFact -> Label -> AllocaFact
    fact f l = fromMaybe 0 $ lookupFact l f
