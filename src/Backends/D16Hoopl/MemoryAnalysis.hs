{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.MemoryAnalysis where
import qualified Data.Map as Map
import Compiler.Hoopl
import Data.Maybe (fromMaybe)
import Instructions (Register(..))
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Debug.Trace

data MemData = Disp Register Int MemSize
  deriving (Show, Eq, Ord)

type MemFact = Map.Map MemData (WithTop Int)
memLattice :: DataflowLattice MemFact
memLattice  = DataflowLattice {
  fact_name = "Memory propogation",
  fact_bot = Map.empty,
  fact_join = joinMaps (extendJoinDomain factJoin) }
  where
    factJoin _ (OldFact old) (NewFact new) =
      if new == old then (NoChange, PElem new)
      else (SomeChange, Top)
        
memRewrite :: forall m. FuelMonad m => FwdRewrite m Node MemFact
memRewrite = mkFRewrite cp
  where
    cp :: Node e x -> MemFact -> m (Maybe (Graph Node e x))
    cp n f =
      return $ fmap insnToG $ mapEEN (replace f) n

    replace :: MemFact -> Expr -> Maybe Expr
    replace f (Load (Binop Add (Reg r) (Lit (Int i))) mf) =
      case Map.lookup (Disp r i mf) f of
        Just (PElem i) -> Just $ Lit $ Int i
        _ -> Nothing
    replace _ _ = Nothing
    mapEEN = mapEN . mapEE

memTransfer = mkFTransfer ft
  where
    ft :: Node e x -> MemFact -> Fact x MemFact
    ft (Label _)            f = f
    ft (Assign _ _)         f = f
    
    ft (Store (Binop Add (Reg r) (Lit (Int i))) (Lit (Int k)) mf) f =
      let d = Disp r i mf
      in Map.insert d (PElem k) f
    ft (Store (Binop Add (Reg r) (Lit (Int i))) _ mf) f =
      Map.insert (Disp r i mf) Top f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase memLattice [(tl,f), (fl,f)]
    ft (None _)             f = f
    ft (Return _)           _ = mapEmpty
