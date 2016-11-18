{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, MultiParamTypeClasses, NamedFieldPuns #-}
module Backends.D16Hoopl.RegisterAllocator where
import Compiler.Hoopl
import qualified Data.Map as Map
import LinearScan.Hoopl
import LinearScan.Hoopl.DSL
import LinearScan
import Data.Maybe
import Instructions (Register(..), intToReg)
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Data.Hashable

import Debug.Trace(trace)

instance HooplNode Node where
    mkBranchNode l = (Branch l)
    mkLabelNode l = (Label l)

instance Show VarKind where
    show Input = "Input"
    show InputOutput = "InputOutput"
    show Temp = "Temp"
    show Output = "Output"
instance Show VarInfo where
    show VarInfo{varId, varKind, regRequired} = 
        case varId of
            Left pr -> (show (intToReg pr)) ++ " " ++ (show varKind) ++ " " ++ (show regRequired)
            Right vi -> (show vi) ++ " " ++ (show varKind) ++ " " ++ (show regRequired)

instance Hashable SVar where
    hashWithSalt s (Svar name i _) = 
       abs $ (hashWithSalt s name) + (hashWithSalt s i)

instance NodeAlloc Node Node where
    isCall (Assign _ (Call _ _)) = True
    isCall (None (Call _ _)) = True
    isCall _ = False
    
    isBranch (Branch _) = True
    isBranch (Cond _ _ _) = True
    
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
    getReferences (Assign (S sv) e) =  
        let lst = fold_EE (svToVInfo Input) [] e
            lst' = svToVInfo Output lst (SVar sv)
        in  lst'
    getReferences (Assign (S v) (Call _ es)) =
        let lst = (foldl . fold_EE) (svToVInfo Input) [] es
            lst' = (svToVInfo Output) lst (SVar v)
            lst'' = lst' ++ [
                VarInfo{varId=Left 0,varKind=Output,regRequired=True},
                VarInfo{varId=Left 1,varKind=Output,regRequired=True},
                VarInfo{varId=Left 2,varKind=Output,regRequired=True},
                VarInfo{varId=Left 3,varKind=Output,regRequired=True}
                ]
        in lst''
    getReferences (Return expr) =
        let lst = foldl (fold_EE (svToVInfo Input)) [] expr
        in  lst
    getReferences node =   
        fold_EN (fold_EE (svToVInfo Input)) [] node
    
    --setRegisters :: [((VarId, VarKind), PhysReg)] -> nv e x -> Env (nr e x)
    setRegisters allocs (Assign sv@(S s) e) =
        let lst = map (\((vid,_),reg) -> (vid,reg)) allocs
            mp = Map.fromList lst
            newExpr = (mapEE . mapSVE) (setRegister mp) e
            expr = case newExpr of
                Just e' -> e'
                Nothing -> e
        in case Map.lookup (hash s) mp of
            Just pr -> return $ (Assign (R (intToReg pr)) expr)
            Nothing -> trace ("Could not allocate dest " ++ (show sv)) return $ (Assign sv expr)
     
    setRegisters allocs node = 
        let lst = map (\((vid,_),reg) -> (vid,reg)) allocs
            mp = Map.fromList lst
            newNode = (mapEN . mapEE . mapSVE) (setRegister mp) node
        in case newNode of
            Nothing -> return node
            Just new -> return new
    
    --mkMoveOps :: PhysReg -> VarId -> PhysReg -> Env [nr O O]
    mkMoveOps r1 _ r2 = return $ (Assign (R (intToReg r2)) (Reg (intToReg r1))):[]
    
    mkSaveOps reg vid = do
        slot <- getStackSlot (Just vid)
        return $ [Store (Binop Add (Reg R6) (Lit (Int slot))) (Reg (intToReg reg))]
        
    
    mkRestoreOps vid reg = do
        slot <- getStackSlot (Just vid)
        return $ [Assign (R (intToReg reg)) (Load (Binop Add (Reg R6) (Lit (Int slot))))]
    
    op1ToString = show
    
svToVInfo :: VarKind -> [VarInfo] -> Expr -> [VarInfo]
svToVInfo vk vi (SVar sv) =  (convert sv):vi
    where 
        convert sv = VarInfo{
            varId = Right $ hash sv,
            varKind = vk,
            regRequired = True
            }
svToVInfo _ vi _ = vi

setRegister :: Map.Map VarId PhysReg -> SVar -> Maybe Expr
setRegister mp sv = 
    case Map.lookup (hash sv) mp of
        Just pr -> Just $ Reg (intToReg pr)
        Nothing -> Nothing



allocate :: Label -> Graph Node C C -> CheckingFuelMonad SimpleUniqueMonad (Graph Node C C)
allocate entry g  = 
    let nRegs = 6
        stackOffset = 2
        regSize = 2
        verifier = VerifyDisabled
        
        (dump,allocated) = allocateHoopl nRegs stackOffset regSize verifier entry g
    in  trace (dump) $
        case allocated of
            Left strs -> return $error $ "Allocation Error: " ++ ( show strs)
            Right gr -> return gr
    
--fold_EE :: (a -> Expr -> a) -> a -> Expr      -> a
