{-# LANGUAGE DeriveDataTypeable, GADTs #-}
module Instructions where
import Numeric
import Data.Char
import Data.Data
import Data.Typeable
data Opcode = Nop | Add | Sub | Push | Pop | Mov | And | Or | Xor | Not | Neg | Ld | St |
    Cmp | Jmp | Call | Ret | Shl | Shr | Rol | Rcl | Ldcp | Stcp | Adc | Sbb | Set | Test|
    Dw | PushLR | Globl | Sar
    deriving (Data, Typeable,Eq)
data Instruction =
    Inst_RR Opcode Register Register |
    Inst_RI Opcode Register Address |
    Inst_R Opcode Register   |
    Inst_I Opcode Address |
    Inst Opcode   |
    Inst_Mem Opcode Register Register ByteFlag  |
    Inst_MemI Opcode Register Register Address ByteFlag DispFlag   |
    Inst_Jmp Opcode Condition Register   |
    Inst_JmpI Opcode Condition Address  |
    Inst_Label String   |
    Inst_Directive Opcode Int

data Address = Label String | Const Int
    deriving (Eq)

data Condition = Nv | Eq | Ne | Os | Oc | Hi | Ls | P | N | Cs | Cc | Ge | G | Le | L | Al
    deriving (Enum, Data, Typeable)
data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
    deriving (Enum,Eq,Ord,Data)
data ByteFlag = Byte | Word
    deriving (Show, Eq)
data DispFlag = Displacement | Constant
    deriving (Show, Eq)

instance Show Condition where
    show x  =
        let upper = showConstr $ toConstr x
        in map toLower upper
instance Show Opcode where
    show Dw = ".dw"
    show x  =
        let upper = showConstr $ toConstr x
        in map toLower upper
instance Show Address where
    show (Label str) = str
    show (Const num)| num >= 0 = "0x" ++ ( showHex num "")
                    | num < 0 = "0x" ++ ( showHex (65536+num) "")

intToReg :: Int -> Register
intToReg 0 = R0
intToReg 1 = R1
intToReg 2 = R2
intToReg 3 = R3
intToReg 4 = R4
intToReg 5 = R5
intToReg 6 = R6
intToReg 7 = R7

regToInt R0 = 0
regToInt R1 = 1
regToInt R2 = 2
regToInt R3 = 3
regToInt R4 = 4
regToInt R5 = 5
regToInt R6 = 6
regToInt R7 = 7
cond_inverse :: Condition -> Condition
cond_inverse Al = Al
cond_inverse Nv = Nv
cond_inverse Eq = Ne
cond_inverse Ne = Eq
cond_inverse Os = Oc
cond_inverse Oc = Os
cond_inverse Hi = Ls
cond_inverse Ls = Hi
cond_inverse P = N
cond_inverse N = P
cond_inverse Ge = L
cond_inverse G = Le
cond_inverse Le = G
cond_inverse L = Ge
cond_inverse Cc = Cs
cond_inverse Cs = Cc
instance Show Register where
    show (R0) = "r0"
    show (R1) = "r1"
    show (R2) = "r2"
    show (R3) = "r3"
    show (R4) = "r4"
    show (R5) = "r5"
    show (R6) = "r6"
    show (R7) = "r7"

instance Show Instruction where
    show (Inst_RR op rD rS) =
        "    " ++ (show op) ++ " " ++ (show rD) ++ ", " ++ (show rS)
    show (Inst_RI op rD (Const num)) =
        "    " ++ (show op) ++ " " ++ (show rD) ++ ", #" ++ (show num)
    show (Inst_RI op rD (Label lbl)) =
        "    " ++ (show op) ++ " " ++ (show rD) ++ ", " ++ lbl
    show (Inst_R op r) =
        "    " ++ (show op) ++ " " ++ (show r)
    show (Inst op) = "    " ++ (show op)
    show (Inst_Jmp op cond reg) =
        "    " ++ (show op) ++ "." ++ (show cond) ++ " " ++ (show reg)
    show (Inst_JmpI op cond num) =
        "    " ++ (show op) ++ "." ++ (show cond) ++ " " ++ (show num)
    show (Inst_Label str) = str ++ ":"
    show (Inst_I op address) =
        "    " ++ (show op) ++ " " ++ (show address)
    show (Inst_Directive Globl _) =
        ".global"
    show (Inst_Directive op val) =
        "    " ++ (show op) ++ " " ++ (showHex val "" )
    show (Inst_Mem op rD rS bf) =
        let regString = case op of
                Ld ->
                    (show rD) ++ ", [" ++ (show rS) ++ "]"
                St ->
                    "[" ++ (show rD) ++ "], " ++ (show rS)
                _ -> error $ "Invalid Instruction: "  ++ (show op)
            opString = case bf of
                Byte -> (show op) ++ ".b"
                Word -> (show op)
        in "    " ++ opString ++ " " ++ regString
    show (Inst_MemI op rD rS num bf df) =
       let opString = case bf of
                Byte -> "    " ++ (show op) ++ ".b"
                Word -> "    " ++ (show op)

        in case df of
            Displacement ->
                case op of
                    Ld ->
                        opString ++ " " ++ (show rD) ++ ", [" ++ (show rS) ++ "+" ++ (show num) ++ "]"
                    St ->
                        opString ++ " [" ++ (show rD) ++ "+" ++ (show num) ++ "], " ++ (show rS)
            Constant ->
                case op of
                    Ld ->
                        opString ++ " " ++ (show rD) ++ ", [" ++ (show num) ++ "]"
                    St ->
                        opString ++ " [" ++ (show num) ++ "], " ++ (show rS)
