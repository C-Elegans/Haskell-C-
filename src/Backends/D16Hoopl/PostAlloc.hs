{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Backends.D16Hoopl.PostAlloc where
import Compiler.Hoopl
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Control.Monad
import Backends.D16Hoopl.OptSupport
import Debug.Trace (trace)

postAlloc :: forall m f. FuelMonad m => FwdRewrite m Node f
postAlloc = mkFRewrite alloc
  where
    alloc :: forall e x. Node e x -> f -> m (Maybe (Graph Node e x))
    alloc node _ = return $ p_node node

    p_node :: Node e x -> Maybe (Graph Node e x)
    p_node node
        | trace ("PostAlloc " ++ show node) False = undefined 
    p_node (Assign (R r) (Binop op (Reg r2) (Reg r3))) 
        | r == r3 && r /= r2 && isAssoc op =
        Just $ mkMiddle (Assign (R r) (Binop op (Reg r3) (Reg r2)))
    p_node (Assign (R r) (Binop Sub (Reg r2) (Reg r3)))
        | r == r3 && r/= r2 =
        Just $ mkMiddles [Assign (R r) (Unop Neg (Reg r)), Assign (R r) (Binop Add (Reg r3) (Reg r2))]
    p_node _ = Nothing
