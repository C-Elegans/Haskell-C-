{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Backends.D16Hoopl.Simplify (simplify) where
import Control.Monad
import Compiler.Hoopl
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.OptSupport
import Debug.Trace(trace)
import Data.Bits


simplify :: forall m f. FuelMonad m => FwdRewrite m Node f
simplify = deepFwdRw simp
  where
    simp :: forall e x. Node e x -> f -> m (Maybe (Graph Node e x))
    simp node _ = return $ liftM insnToG $ s_node node
    
    s_node :: Node e x -> Maybe (Node e x)
    s_node n 
        | trace ("s_node " ++ (show n)) False = undefined
    s_node (Cond (Lit (Bool b)) t f) =
        Just $ Branch (if b then t else f)
    s_node (Assign (R r1) (Binop op (Reg r2) (Reg r3))) 
        | r1 == r3 && isAssoc op =
        Just $ (Assign (R r1) (Binop op (Reg r3) (Reg r2)))
    
    s_node n = (mapEN . mapEE) s_exp n
    s_exp (Binop opr e1 e2) 
        | (Just op, Lit (Int i1), Lit (Int i2)) <- (intOp opr, e1, e2) =
            Just $ Lit $ Int $ op i1 i2
        | (Just op, Lit (Int i1), Lit (Int i2)) <- (cmpOp opr, e1, e2) =
            Just $ Lit $ Bool $ op i1 i2
    s_exp (Binop op (Lit (Int i)) e1) 
        | isAssoc op = Just $ Binop op e1 (Lit (Int i))
    s_exp (Binop Mul e1 (Lit (Int i))) =
        let trailing = (countTrailingZeros i)
            remainder = shiftR i trailing
        in if trailing > 0 && i /= 0 then
                Just (Binop Mul (Binop Shl e1 (Lit (Int trailing))) (Lit (Int remainder)))
            else 
                case i of
                    0 -> Just $ Lit $ Int 0
                    1 -> Just e1
                    _ -> Nothing
    
    s_exp (Unop opr e1)
        | (Just op, Lit (Int i1)) <- (unOp opr, e1) =
        Just $ Lit $ Int $ op i1
    s_exp (Call _ _) = Nothing
        
        
        
    s_exp _ = Nothing
    intOp Add = Just (+)
    intOp Sub = Just (-)
    intOp Mul = Just (*)
    intOp Div = Just div
    intOp And = Just (.&.)
    intOp Or  = Just (.|.)
    intOp Xor = Just xor
    intOp Shl = Just shiftL
    intOp Shr = Just shiftR
    intOp Sar = Just shiftR
    intOp _   = Nothing
    cmpOp Eq  = Just (==)
    cmpOp Ne  = Just (/=)
    cmpOp Gt  = Just (>)
    cmpOp Lt  = Just (<)
    cmpOp Gte = Just (>=)
    cmpOp Lte = Just (<=)
    cmpOp _   = Nothing
    unOp Not  = Just complement
    unOp Neg  = Just (0-)
    unOp _    = Nothing
