{-# LANGUAGE ScopedTypeVariables, GADTs, MultiParamTypeClasses, NamedFieldPuns #-}

module Backends.D16Hoopl.IRToInstruction where
import Compiler.Hoopl
import Control.Monad.Writer
import Backends.D16Hoopl.IR hiding (Label,Call)
import qualified Backends.D16Hoopl.IR as IR
import qualified Backends.D16Hoopl.Expr as E
import Compiler.Hoopl.Internals
import Backends.D16Hoopl.Instructions
import Backends.D16Hoopl.GraphUtils
import Debug.Trace (trace)

assembleNodeCO :: UniqueMonad m => Node C O -> m (Graph Instruction C O)
assembleNodeCO (IR.Label lbl) = 
    return $ mkFirst $ Inst_Label "lbl"
assembleNodeOO :: UniqueMonad m => Node O O -> m (Graph Instruction O O)
assembleNodeOO (Assign r e) =
    return $emptyGraph
    
assembleNodeOC :: UniqueMonad m => Node O C -> m (Graph Instruction O C)
assembleNodeOC n = 
    return $ mkLast $ Inst_Jmp Jmp Al R0

--assembleFunction :: Proc -> Graph Instruction C C
assembleFunction proc = body proc
   --mapConcatGraph (assembleNodeCO, assembleNodeOO, assembleNodeOC) (body proc)

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
