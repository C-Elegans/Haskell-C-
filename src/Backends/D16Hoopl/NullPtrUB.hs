{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.NullPtrUB where
import qualified Data.Map as Map
import Compiler.Hoopl
import Debug.Trace (trace)
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
data IsNull = Null | Nonnull | Unknown deriving (Eq,Show)
instance Ord IsNull where
    Unknown <= _ = False
    Nonnull <= Null = False
    _ <= _ = True
type NullPtrFact  = Map.Map SVar IsNull
nullLattice :: DataflowLattice NullPtrFact
nullLattice = DataflowLattice {
    fact_name = "Null Pointer Map",
    fact_bot = Map.empty,
    fact_join = add }
    where
        add _ (OldFact old) (NewFact new) =
            let map = Map.unionWith max old new 
                changeFlag = changeIf  False
            in (changeFlag, map)
            

nullTransfer :: FwdTransfer Node NullPtrFact
nullTransfer = mkFTransfer ft
  where
    ft :: Node e x -> NullPtrFact -> Fact x NullPtrFact
    ft (Label _)                              f = f
    ft (Assign (S sv)  (Lit (Int 0)))         f = Map.insert sv Null f
    ft (Assign _ (Load (SVar sv)))            f = Map.insert sv Nonnull f
    ft (Assign _ _)                           f = f

    ft (Store (SVar sv) _)                    f = Map.insert sv Nonnull f
    ft (Store _ _)                            f = f
    ft (Branch l)                             f = mapSingleton l f
    ft (Cond _ tl fl)                         f =
        mkFactBase nullLattice [(tl,f), (fl,f)]
    ft (None _)                               f = f
    ft (Return _)                             _ = mapEmpty
    



nullRewrite :: forall m . FuelMonad m => FwdRewrite m Node NullPtrFact
nullRewrite = mkFRewrite rw
  where
    rw :: Node e x -> NullPtrFact -> m (Maybe (Graph Node e x))
    rw node f =
        return $ fmap insnToG $ (mapEN . mapEE) (lookup f) node
    
    lookup :: NullPtrFact -> Expr -> Maybe Expr
    lookup f n 
        | trace ("nullptr " ++ show n ++ " Fact " ++ show f) False = undefined
    lookup f (Binop op (SVar sv) (SVar sv2)) =
        do
            opr <- cmpOp op
            n1 <- Map.lookup sv f
            n2 <- Map.lookup sv2 f
            if n2 == Null then
               case n1 of
                    Null -> Just $ Lit $ Bool $ opr 0 0
                    Nonnull -> Just $ Lit $ Bool $ opr 1 0
                    _ -> Nothing
            else
               Nothing
    lookup _ _ = Nothing
    cmpOp Eq  = Just (==)
    cmpOp Ne  = Just (/=)
    cmpOp Gt  = Just (>)
    cmpOp Lt  = Just (<)
    cmpOp Gte = Just (>=)
    cmpOp Lte = Just (<=)
    cmpOp _   = Nothing
