{-# LANGUAGE ScopedTypeVariables, GADTs, MultiParamTypeClasses, NamedFieldPuns #-}

module Backends.D16Hoopl.IRToInstruction where
import Compiler.Hoopl
import Control.Monad.Writer
import Backends.D16Hoopl.IR hiding (Label,Call)
import Backends.D16Hoopl.GraphUtils (foldGraphNodes')
import qualified Backends.D16Hoopl.IR as IR
import qualified Backends.D16Hoopl.Expr as E
import Compiler.Hoopl.Internals
import Instructions

import Debug.Trace (trace)

type Program = [Instruction]

emitPrologue :: String -> Writer Program ()
emitPrologue fname = do
    tell $ [Inst_Directive Globl 0]
    tell $ [Inst_Label fname]
    tell $ [Inst PushLR]
    tell $ [Inst_R Push R6]
    tell $ [Inst_RR Mov R6 R7]
    tell $ [Inst_JmpI Jmp Al (Label ("_L1_" ++ fname))]
    return ()

epilogue = [Inst_RR Mov R7 R6, Inst_R Pop R6,Inst_R Pop R1, Inst_Jmp Jmp Al R1]
emitEpilogue :: Writer Program ()   
emitEpilogue = do
    tell $ epilogue
    return ()

append :: [a] -> [a] -> [a]
append src dest = 
    --(reverse src)++ dest
    src ++ dest
    
assemble :: Proc -> Program
assemble p = execWriter (assembleFunction p)
--The fold over the graph is backwards for some reason. 

assembleNode :: String -> Node e x -> [Instruction] -> [Instruction]
assembleNode _ (Assign (R r) (E.Binop op (E.Reg r2) (E.Reg r3))) 
    | canBeOp op =
    if r == r2 then
        append [(Inst_RR (binopToOp op) r2 r3)]
    else
        append [(Inst_RR Mov r r2),(Inst_RR (binopToOp op) r r3)] 
assembleNode _(Assign (R r) (E.Binop op (E.Reg r2) (E.Lit (E.Int i)))) 
    | canBeOp op =
    if r == r2 then
        append [instruction] 
    else
        append [(Inst_RR Mov r r2),instruction] 
        where
            instruction = (Inst_RI (binopToOp op) r (Const i))
assembleNode _(Assign (R r) (E.Binop op (E.Reg r2) (E.Lit (E.Int i)))) =
    append [Inst_RI Cmp r2 (Const i), Inst_Jmp Set (binopToCond op) r]
assembleNode _(Assign (R r) (E.Binop op (E.Reg r2) (E.Reg r3))) =
    append [Inst_RR Cmp r2 r3, Inst_Jmp Set (binopToCond op) r]
assembleNode _ (Assign (R r) (E.Lit (E.Int i))) =
    append [Inst_RI Mov r (Const i)]
assembleNode _ (Assign (R r) (E.Reg r2)) = 
    append [Inst_RR Mov r r2]
assembleNode _ (Assign (R r) (E.Load (E.Binop E.Add (E.Reg R6) (E.Lit (E.Int i))))) =
    append [Inst_MemI Ld r R6 (Const i) Word Displacement]
assembleNode _ (Store (E.Binop E.Add (E.Reg R6) (E.Lit (E.Int i))) (E.Reg r)) =
    append [Inst_MemI St R6 r (Const i) Word Displacement]
assembleNode _ (Store (E.Reg r) (E.Reg r2)) =
    append [Inst_Mem St r r2 Word]
assembleNode _ (Assign (R r) (E.Load (E.Reg r2))) = 
    append [Inst_Mem Ld r r2 Word]
assembleNode _(Return ((E.Reg r):[]))  = 
    if r /= R0 then
        append ((Inst_RR Mov R0 r):epilogue) 
    else
        append epilogue
assembleNode _ (Return ((E.Lit (E.Int i)):[])) = 
    append $ (Inst_RI Mov R0 (Const i)):epilogue
assembleNode _ (Return _) =
    append epilogue
--assembleNode _ (IR.Call [] name exprs) = --Need to check register order
    --append [Inst_JmpI Call Al (Label name)]
assembleNode name (IR.Label lbl) =
    append [Inst_Label (lblToLabel lbl name)]

assembleNode name (Branch lbl) =
    append [Inst_JmpI Jmp Al (Label (lblToLabel lbl name))]
assembleNode name (Cond (E.Reg r) tl fl) =
    append [Inst_RR Test r r,
            Inst_JmpI Jmp Ne (Label (lblToLabel tl name)),
            Inst_JmpI Jmp Al (Label (lblToLabel fl name))]
assembleNode _ n = error $ "No assembleNode defined for " ++ (show n)


assembleFunction :: Proc -> Writer Program ()
assembleFunction proc = do
    emitPrologue $ name proc
    tell $ foldGraphNodes' (assembleNode (name proc)) (body proc) []
    
    return ()

lblToLabel :: Label -> String -> String
lblToLabel lbl name =  ("_" ++ (show lbl) ++ "_" ++ name)


binopToOp :: E.BinOp -> Opcode
binopToOp E.Add = Add
binopToOp E.Sub = Sub
binopToOp E.And = And
binopToOp E.Or  = Or
binopToOp E.Xor = Xor
binopToOp E.Shl = Shl
binopToOp E.Shr = Shr


canBeOp E.Add = True
canBeOp E.Sub = True
canBeOp E.And = True
canBeOp E.Or  = True
canBeOp E.Xor = True
canBeOp E.Shl = True
canBeOp E.Shr = True
canBeOp _ = False

binopToCond E.Ne = Ne
binopToCond E.Eq = Eq
binopToCond E.Gt = G
binopToCond E.Gte = Ge
binopToCond E.Lt = L
binopToCond E.Lte = Le
