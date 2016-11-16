{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, MultiParamTypeClasses #-}
module Backends.D16Hoopl.RegisterAllocator where
import Compiler.Hoopl
import LinearScan.Hoopl
import LinearScan
import Instructions (intToReg)
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Data.Hashable

import Debug.Trace(trace)

instance HooplNode Node where
    mkBranchNode l = (Branch l)
    mkLabelNode l = (Label l)



instance NodeAlloc Node Node where
    isCall _ = False
    
    isBranch (Branch _) = True
    isBranch (Cond _ _ _) = True
    isBranch (Call _ _ _ _) = True
    isBranch _ = False
    
    retargetBranch (Branch _) _ lbl = (Branch lbl)
    retargetBranch (Cond c tl fl) old new =
        if tl == old then
            (Cond c new fl)
        else
            (Cond c tl new)
            
    retargetBranch node _ _ = node

    mkLabelOp lbl = (Label lbl)
    mkJumpOp lbl = (Branch lbl)
    
    getReferences = fold_EN (fold_EE svToVInfo) []
    
    --setRegisters :: [((VarId, VarKind), PhysReg)] -> nv e x -> Env (nr e x)
    setRegisters allocs node = return node
    
    --mkMoveOps :: PhysReg -> VarId -> PhysReg -> Env [nr O O]
    mkMoveOps r1 _ r2 = return $ (Assign (R (intToReg r2)) (Reg (intToReg r1))):[]
    
    mkSaveOps reg vid = return $ (Assign (S (Svar "tmp" vid S_None)) (Reg (intToReg reg))):[]
    
    mkRestoreOps vid reg = return $ (Assign (R (intToReg reg)) (SVar (Svar "tmp" vid S_None))):[]
    
    op1ToString = show
    
svToVInfo :: [VarInfo] -> Expr -> [VarInfo]
svToVInfo vi (SVar sv) = (convert sv):vi
    where 
        convert (Svar name i _) = VarInfo{
            varId = Right $ hash (name ++ (show i)),
            varKind = Input,
            regRequired = True
            }
svToVInfo vi _ = vi



allocate :: Label -> Graph Node C C -> CheckingFuelMonad SimpleUniqueMonad (Graph Node C C)
allocate entry g  = 
    let nRegs = 6
        stackOffset = 16
        regSize = 2
        verifier = VerifyEnabled
        
        (dump,allocated) = allocateHoopl nRegs stackOffset regSize verifier entry g
    in  trace (dump) $
        case allocated of
            Left strs -> return $error $ "Allocation Error: " ++ ( show strs)
            Right gr -> return gr
    
--fold_EE :: (a -> Expr -> a) -> a -> Expr      -> a
