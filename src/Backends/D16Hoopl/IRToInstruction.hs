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
data FunctionType = Leaf | Stem deriving (Eq,Show) -- leaf functions don't need to save the link register
emitPrologue :: String -> FunctionType -> Writer Program ()
emitPrologue fname ftype = do
    tell $ [Inst_Directive Globl 0]
    tell $ [Inst_Label ("_" ++ fname)]
    if ftype == Stem then
        tell $ [Inst PushLR]
    else 
        tell $ []
    tell $ [Inst_R Push R6]
    tell $ [Inst_RR Mov R6 R7]
    tell $ [Inst_RI Sub R7 (Const 16)]
    tell $ [Inst_JmpI Jmp Al (Label ("_L1_" ++ fname))]
    return ()

epilogue = [Inst_RR Mov R7 R6, Inst_R Pop R6,Inst_R Pop R1, Inst_Jmp Jmp Al R1]
epilogueLeaf = [Inst_RR Mov R7 R6, Inst_R Pop R6, Inst Ret]
emitEpilogue ::FunctionType -> Writer Program ()   
emitEpilogue ftype = do
    if ftype == Stem then
        tell epilogue
    else
        tell epilogueLeaf
    return ()

append :: [a] -> [a] -> [a]
append src dest = 
    --(reverse src)++ dest
    src ++ dest
    
assemble :: Proc -> Program
assemble p = execWriter (assembleFunction p)
--The fold over the graph is backwards for some reason. 

assembleNode :: String -> FunctionType -> Node e x -> [Instruction] -> [Instruction]
assembleNode _ _ node@(Assign (R r) (E.Binop op (E.Reg r2) (E.Reg r3))) 
    | canBeOp op =
    if r == r2 then
        append [(Inst_RR (binopToOp op) r2 r3)]
    else
        if r == r3 then
            (\_ ->error $ "Cannot convert to 2 address code "++ (show node))
        else
            append [(Inst_RR Mov r r2),(Inst_RR (binopToOp op) r r3)] 
assembleNode _ _(Assign (R r) (E.Binop op (E.Reg r2) (E.Lit (E.Int i)))) 
    | canBeOp op =
    if r == r2 then
        append [instruction] 
    else
        append [(Inst_RR Mov r r2),instruction] 
        where
            instruction = (Inst_RI (binopToOp op) r (Const i))
assembleNode _ _(Assign (R r) (E.Binop op (E.Reg r2) (E.Lit (E.Int i)))) =
    append [Inst_RI Cmp r2 (Const i), Inst_Jmp Set (binopToCond op) r]
assembleNode _ _(Assign (R r) (E.Binop op (E.Reg r2) (E.Reg r3))) =
    append [Inst_RR Cmp r2 r3, Inst_Jmp Set (binopToCond op) r]
assembleNode _ _(Assign (R r) (E.Unop op (E.Reg r2))) =
    append [Inst_R (unopToOp op) r2, Inst_RR Mov r r2]
assembleNode _ _ (Assign (R r) (E.Lit (E.Int i))) =
    append [Inst_RI Mov r (Const i)]
assembleNode _ _ (Assign (R r) (E.Str str)) =
    append [Inst_RI Mov r (Label str)]
assembleNode _ _ (Assign (R r) (E.Reg r2)) = 
    append [Inst_RR Mov r r2]
assembleNode _ _ (Assign (R r) (E.Load (E.Binop E.Add (E.Reg R6) (E.Lit (E.Int i))))) =
    append [Inst_MemI Ld r R6 (Const i) Word Displacement]
assembleNode _ _ (Store (E.Binop E.Add (E.Reg R6) (E.Lit (E.Int i))) (E.Reg r)) =
    append [Inst_MemI St R6 r (Const i) Word Displacement]
assembleNode _ _ (Store (E.Reg r) (E.Reg r2)) =
    append [Inst_Mem St r r2 Word]
assembleNode _ _ (Assign (R r) (E.Load (E.Reg r2))) = 
    append [Inst_Mem Ld r r2 Word]
assembleNode _ ftype  (Return ((E.Reg r):[])) = 
    let ep = case ftype of
                Stem -> epilogue
                Leaf -> epilogueLeaf
    in if r /= R0 then
        append ((Inst_RR Mov R0 r):ep) 
    else
        append ep
assembleNode _ ftype (Return ((E.Lit (E.Int i)):[])) = 
    case ftype of
        Stem ->
            append $ (Inst_RI Mov R0 (Const i)):epilogue
        Leaf ->
            append $ (Inst_RI Mov R0 (Const i)):epilogueLeaf
assembleNode _ ftype (Return _) =
    case ftype of
        Stem -> append epilogue
        Leaf -> append epilogueLeaf
assembleNode _ _ (None (E.Call name rs)) =
    append [Inst_JmpI Call Al (Label ("_" ++ name))]
assembleNode _ _ (Assign (R R0) (E.Call name rs)) =
    append [Inst_JmpI Call Al (Label ("_" ++ name))]
assembleNode _ _ (None _) =
    id
assembleNode name _ (IR.Label lbl) =
    append [Inst_Label (lblToLabel lbl name)]

assembleNode name _ (Branch lbl) =
    append [Inst_JmpI Jmp Al (Label (lblToLabel lbl name))]
assembleNode name _ (Cond (E.Reg r) tl fl) =
    append [Inst_RR Test r r,
            Inst_JmpI Jmp Ne (Label (lblToLabel tl name)),
            Inst_JmpI Jmp Al (Label (lblToLabel fl name))]
assembleNode _ _ n = error $ "No assembleNode defined for " ++ (show n)


assembleFunction :: Proc -> Writer Program ()
assembleFunction proc = do
    let fType = foldGraphNodes' (isLeaf) (body proc) Leaf 
    emitPrologue  (name proc) fType
    tell $ foldGraphNodes' (assembleNode (name proc) fType) (body proc) []
    
    return ()

isLeaf :: Node e x -> FunctionType -> FunctionType
isLeaf (Assign _ (E.Call _ _)) ft = Stem
isLeaf (None (E.Call _ _)) ft = Stem
isLeaf _ ft = ft
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

unopToOp :: E.UnOp -> Opcode
unopToOp E.Neg = Neg
unopToOp E.Not = Not

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
binopToCond c = error $ "No binopToCond defined for " ++ show c
