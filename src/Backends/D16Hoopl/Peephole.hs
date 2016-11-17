module Backends.D16Hoopl.Peephole (runPeephole) where
import Instructions

passes = [
        move_constants,
        condense_compare,
        eliminate_redundant_jump
    ]

runPeephole :: [Instruction] -> [Instruction]
runPeephole = run passes

run :: [([Instruction] -> [Instruction])] -> [Instruction] -> [Instruction]
run (pass:passes) insts = 
    run passes (pass insts)
    
run [] insts = insts

eliminate_redundant_jump ((Inst_JmpI Jmp Al (Label l)):(Inst_Label lbl):rest)
    | l == lbl =
        (Inst_Label lbl):(eliminate_redundant_jump rest)

eliminate_redundant_jump ((Inst_JmpI Jmp cond (Label l1)):(Inst_JmpI Jmp Al (Label l2)):(Inst_Label l3):rest)
    | l1 == l3 =
        (Inst_JmpI Jmp(cond_inverse cond) (Label l2)):
        (eliminate_redundant_jump $ (Inst_JmpI Jmp Al (Label l1)):(Inst_Label l3):(rest))
eliminate_redundant_jump (x:xs) = x:(eliminate_redundant_jump xs)
eliminate_redundant_jump [] = []


condense_compare ((Inst_Jmp Set cond r1):(Inst_RR Test r2 r3):(Inst_JmpI Jmp Ne (Label l)):rest) 
    | r1 == r2 && r2 == r3 =
        (Inst_JmpI Jmp cond (Label l)):(condense_compare rest)
condense_compare (x:xs) = x:(condense_compare xs)
condense_compare [] = []

move_constants ((Inst_RI Mov r1 (Const i)):(Inst_RR op r2 r3):rest) 
    | r1 == r3 =
    (Inst_RI op r2 (Const i)):(move_constants rest)
move_constants (x:xs) = x:(move_constants xs)
move_constants [] = []
