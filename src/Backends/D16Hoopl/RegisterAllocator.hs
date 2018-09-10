{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, MultiParamTypeClasses, NamedFieldPuns #-}
module Backends.D16Hoopl.RegisterAllocator where
import Compiler.Hoopl
import qualified Data.Map as Map
import LinearScan.Hoopl
import LinearScan.Hoopl.DSL
import LinearScan
import Data.Maybe
import Instructions (Register(..), intToReg, regToInt)
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Data.Hashable

import Debug.Trace(trace)

{-
 -Performs register allocation through the use of the Linearscan-hoopl library.
 -}

instance HooplNode Node where
    mkBranchNode = Branch
    mkLabelNode  = Label

instance Show VarKind where
    show Input       = "Input"
    show InputOutput = "InputOutput"
    show Temp        = "Temp"
    show Output      = "Output"
instance Show VarInfo where
    show VarInfo{varId, varKind, regRequired} = 
        case varId of
            Left pr  -> (show (intToReg pr)) ++ " " ++ (show varKind) ++ " " ++ (show regRequired)
            Right vi -> (show vi) ++ " " ++ (show varKind) ++ " " ++ (show regRequired)

--Linearscan requires variable IDs to be integers, so we define a hashing function for SVars
instance Hashable SVar where
    hashWithSalt s (Svar name i _) = 
       abs $ (hashWithSalt s name) + (hashWithSalt s i)

instance NodeAlloc Node Node where
    --It seems that Linearscan does unneccesary spills if these are marked as calls
    isCall (Assign _ (Call _ _)) = False
    isCall (None (Call _ _))     = False
    isCall _                     = False
    
    isBranch (Branch _)   = True
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
    mkJumpOp lbl  = (Branch lbl)

    --Returns all variables and registers referenced by the given node
    {-getReferences n | trace ("getReferences " ++ show n) False = undefined-}
    getReferences (Assign (R _) (Call name es)) 
        | name `elem` ["mul","div","abs"] =
        let lst   = (foldl . fold_EE) (svToVInfo Input) [] es
            --This ensures that linearscan knows r0-r1 are not preserved across special calls.
            lst'' = lst ++ [
                VarInfo{varId=Left 0,varKind=Output,regRequired=True},
                VarInfo{varId=Left 1,varKind=Output,regRequired=True}
                ]
        in lst''
    getReferences (Assign (R _) (Call _ es)) =
        let lst   = (foldl . fold_EE) (svToVInfo Input) [] es
            --This ensures that linearscan knows r0-r3 are not preserved across calls.
            lst'' = lst ++ [
                VarInfo{varId=Left 0,varKind=Output,regRequired=True},
                VarInfo{varId=Left 1,varKind=Output,regRequired=False},
                VarInfo{varId=Left 2,varKind=Output,regRequired=False},
                VarInfo{varId=Left 3,varKind=Output,regRequired=False}
                ]
        in lst''
    getReferences (Assign (S sv) e) =  
        let lst  = fold_EE (svToVInfo Input) [] e
            lst' = svToVInfo Output lst (SVar sv)
        in  lst'
    getReferences (Return expr) =
        let lst = foldl (fold_EE (svToVInfo Input)) [] expr
        in  lst
    getReferences node =   
        fold_EN (fold_EE (svToVInfo Input)) [] node
    
    --Applies register allocations to the given node
    setRegisters allocs (Assign sv@(S s) e) =
        let lst     = map (\((vid,_),reg) -> (vid,reg)) allocs
            mp      = Map.fromList lst
            newExpr = (mapEE . mapSVE) (setRegister mp) e
            expr    = fromMaybe e newExpr
        in case Map.lookup (hash s) mp of
            Just pr -> return $ Assign (R (intToReg pr)) expr
            Nothing -> trace ("Could not allocate dest " ++ show sv) return (Assign sv expr)
     
    setRegisters allocs node = 
        let lst     = map (\((vid,_),reg) -> (vid,reg)) allocs
            mp      = Map.fromList lst
            newNode = (mapEN . mapEE . mapSVE) (setRegister mp) node
        in case newNode of
            Nothing  -> return node
            Just new -> return new
    
    --moves a register to another register
    mkMoveOps r1 _ r2 = return [Assign (R (intToReg r2)) (Reg (intToReg r1))]
    
    --saves a register to a stack slot
    mkSaveOps reg vid = do
        slot <- getStackSlot (Just vid)
        return [Store (Binop Add (Reg R6) (Lit (Int (-slot)))) (Reg (intToReg reg)) Word]
        
    --restores a register from a stack slot 
    mkRestoreOps vid reg = do
        slot <- getStackSlot (Just vid)
        return [Assign (R (intToReg reg)) (Load (Binop Add (Reg R6) (Lit (Int (-slot)))) Word)]
    
    op1ToString = show

--Function mapped over nodes in getReferences
svToVInfo :: VarKind -> [VarInfo] -> Expr -> [VarInfo]
svToVInfo vk vi (Reg r) = (convert r):vi
    where
        convert r = VarInfo{
            varId = Left $ regToInt r,
            varKind = vk,
            regRequired = True
        }
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
    let nRegs       = 6
        stackOffset = 4
        regSize     = 2
        verifier    = VerifyEnabled
        
        (_,allocated) = allocateHoopl nRegs stackOffset regSize verifier entry g
    in  
        case allocated of
            Left strs -> return $error $ "Allocation Error: " ++ show strs
            Right gr  -> return gr
    
