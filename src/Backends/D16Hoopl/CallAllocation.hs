{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Backends.D16Hoopl.CallAllocation where
import Compiler.Hoopl
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.OptSupport
import Control.Monad
import Instructions (Register(..))
import Prelude hiding ((<*>))

{-
 -Performs register allocation for call parameters. At the suggestion of John Wiegly, the author
 -of linearscan and linearscan-hoopl, calling conventions should be implemented by assigning call
 -parameters to their appropriate registers or stack slots before allocation takes place
 -}

type CallFact = Int
emptyLattice :: DataflowLattice CallFact
emptyLattice = DataflowLattice {
    fact_name = "Empty Fact",
    fact_bot = 0,
    fact_join = add }
    where
        add _ (OldFact old) (NewFact new) =
            let map = old + new 
                changeFlag = changeIf  False
            in (changeFlag, map)
            

emptyTransfer :: FwdTransfer Node CallFact
emptyTransfer = mkFTransfer ft
  where
    ft :: Node e x -> CallFact -> Fact x CallFact
    ft (Label _)            f = f
    ft (Assign v _)         f = f
    
    ft (Store _ _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase emptyLattice [(tl,f), (fl,f)]
    ft (None _)             f = f
    ft (Return _)           _ = mapEmpty
    
callAlloc :: forall m f. FuelMonad m => FwdRewrite m Node f
callAlloc = mkFRewrite alloc
  where
    alloc :: forall e x. Node e x -> f -> m (Maybe (Graph Node e x))
    alloc node _ = return $ a_node node
    
    call_registers = [R0, R1, R2, R3]
    a_node :: Node e x -> Maybe (Graph Node e x)
    a_node (None (Call name args)) =
       let nodes = assign_args call_registers args
           args' = map (\r -> Reg r) (take (length args) call_registers)
       in Just $ (mkMiddles nodes) <*> mkMiddle (None (Call name args'))
    a_node (Assign (S s) (Call "mod" args)) =
        let nodes = assign_args call_registers args
            args' = map (Reg) (take (length args) call_registers)
        in  Just $  (mkMiddles nodes) <*> 
                    (mkMiddle (Assign (R R1) (Call "div" args'))) <*>
                    (mkMiddle (Assign (S s) (Reg R1)))
    a_node (Assign (S s) (Call name args)) =
        let nodes = assign_args call_registers args
            args' = map (Reg) (take (length args) call_registers)
        in  Just $  (mkMiddles nodes) <*> 
                    (mkMiddle (Assign (R R0) (Call name args'))) <*>
                    (mkMiddle (Assign (S s) (Reg R0)))
    a_node (Return ((SVar sv):[])) =
        Just $ (mkMiddle (Assign (R R0) (SVar sv))) <*> mkLast (Return [Reg R0])
    a_node _ = Nothing

    assign_args :: [Register] -> [Expr] -> [Node O O]
    assign_args (reg:rs) (var:vs) =
        Assign (R reg) var :assign_args rs vs
    assign_args _ [] = []
    assign_args [] v = error $ "Not enough registers for var " ++ show v
