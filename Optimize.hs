module Optimize where
import Stack (run_stack_analysis)
import Instructions
passes = [run_stack_analysis,peephole_1,peephole_2,const_fold,load_store,redundant_ret,indexed_mem]

optimize :: [Instruction] -> [Instruction]
optimize = run passes

run :: [([Instruction]->[Instruction])] -> [Instruction] -> [Instruction]
run (pass:passes) instructions = 
    run passes (pass instructions)
run [] insts = insts

peephole_1 :: [Instruction] -> [Instruction]

peephole_1 ((Inst_R Push reg1):(Inst_R Pop reg2):rest) =
    (Inst_RR Mov reg2 reg1):(peephole_1 rest)

peephole_1 (x:xs) = x:(peephole_1 xs)
peephole_1 [] = []

peephole_2 (i1@(Inst_RR op reg1 reg2):i2@(Inst_RR Mov reg4 reg3):rest) =
    if op `elem` [Add, And, Adc, Or, Xor] && reg1 == reg3 && reg2 == reg4 then
        (Inst_RR op reg2 reg1):(peephole_2 rest)
    else
        i1:i2:(peephole_2 rest)

peephole_2 (x:xs) = x:(peephole_2 xs)
peephole_2 [] = []

const_fold ((Inst_RI Mov reg1 const):(Inst_RR op reg2 reg3):rest) 
    | reg1 == reg3 =
        (Inst_RI op reg2 const):(const_fold rest)

const_fold (x:xs) = x:(const_fold xs)
const_fold [] = []

load_store (i1@(Inst_MemI St rB rS const bf Displacement):(Inst_MemI Ld rD rB2 const2 bf2 Displacement):rest)
    | rB == rB2 && rS == rD && const == const2 && bf == bf2 =
    (i1:(load_store rest))

load_store (x:xs) = x:(load_store xs)
load_store [] = []

redundant_ret ((Inst Ret):(Inst_RR Mov R7 R6):(Inst_R Pop R6):(Inst Ret):rest) = 
    (Inst Ret):(redundant_ret rest)
redundant_ret (x:xs) = x:(redundant_ret xs)
redundant_ret [] = []

indexed_mem ((Inst_RI Add reg1 (Const v)):i2@(Inst_RI op reg2 val2):(Inst_Mem St reg3 reg4 bf):rest)
    | reg1 == reg3 && reg2 == reg4 && reg1 /= reg2
    = i2:(Inst_MemI St reg1 reg2 (Const v) bf Displacement):(indexed_mem rest)
        

indexed_mem (x:xs) = x:(indexed_mem xs)
indexed_mem [] = []
