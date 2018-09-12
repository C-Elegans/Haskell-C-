{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Backends.D16Hoopl.DeadStore where
import Data.Maybe
import Compiler.Hoopl
import Instructions (Register(..))
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.OptSupport
import qualified Data.Set as S


data Loc = Disp Register Int MemSize
  deriving (Show, Eq, Ord)

type LiveMem = S.Set Loc

dsLattice :: DataflowLattice LiveMem
dsLattice = DataflowLattice {
  fact_name = "Live Memory Locations",
  fact_bot = S.empty, 
  fact_join = add}
  where
    add _ (OldFact old) (NewFact new) = (ch, j)
      where
        j = new `S.union` old
        ch = changeIf (S.size j > S.size old)


liveStoreTransfer = mkBTransfer3 fl ml ll
  where
    fl :: Node C O -> LiveMem -> LiveMem
    
    fl (Label _)            f = f

    ml :: Node O O -> LiveMem -> LiveMem
    ml (Assign _ e)         f =
      fold_EE adduses f e
    ml (Store (Binop Add (Reg r) (Lit (Int i))) _ mf) f =
      let l = Disp r i mf
      in S.delete l f
    ml (Store _ _ _)        f = f
    ml (None _)             f = f

    ll :: Node O C -> FactBase LiveMem -> LiveMem
    ll (Branch l)           f = fact f l
    ll (Cond _ tl fl)       f =
        fact f tl `S.union` fact f fl
    ll (Return _)           _ = fact_bot dsLattice

    fact :: FactBase (S.Set Loc) -> Label -> LiveMem
    fact f l = fromMaybe S.empty $ lookupFact l f
    adduses :: LiveMem -> Expr -> LiveMem
    adduses f (Load (Binop Add (Reg r) (Lit (Int i))) mf) =
      let d = Disp r i mf
      in S.insert d f
    adduses f _ = f


liveStoreRw :: forall m. FuelMonad m => BwdRewrite m Node LiveMem
liveStoreRw = mkBRewrite del
  where
    del :: Node e x -> Fact x LiveMem -> m (Maybe (Graph Node e x))
    del (Store (Binop Add (Reg r) (Lit (Int i))) _ mf) f =
      case S.member (Disp r i mf) f of
        True -> return Nothing
        False -> return $ Just emptyGraph
    del _  _ = return $ Nothing
