module Backends.D16Hoopl.Peephole (runPeephole) where
import Instructions

passes = [
        move_constants,
        condense_compare,
        eliminate_redundant_jump,
        condense_loads,
        reduce_instruction_size
    ]

runPeephole :: [Instruction] -> [Instruction]
runPeephole = run passes

run :: [[Instruction] -> [Instruction]] -> [Instruction] -> [Instruction]
run (pass:passes) insts = 
    run passes (pass insts)
    
run [] insts = insts

eliminate_redundant_jump ((Inst_JmpI Jmp Al (Label l)):(Inst_Label lbl):rest)
    | l == lbl =
        Inst_Label lbl:eliminate_redundant_jump rest

eliminate_redundant_jump ((Inst_JmpI Jmp cond (Label l1)):(Inst_JmpI Jmp Al (Label l2)):(Inst_Label l3):rest)
    | l1 == l3 =
        (Inst_JmpI Jmp(cond_inverse cond) (Label l2)):
        (eliminate_redundant_jump $ (Inst_JmpI Jmp Al (Label l1)):(Inst_Label l3):(rest))
eliminate_redundant_jump (x:xs) = x:(eliminate_redundant_jump xs)
eliminate_redundant_jump [] = []


condense_compare ((Inst_Jmp Set cond r1):(Inst_RR Test r2 r3):(Inst_JmpI Jmp Ne (Label l)):rest) 
    | r1 == r2 && r2 == r3 =
        Inst_JmpI Jmp cond (Label l):condense_compare rest
condense_compare (x:xs) = x:condense_compare xs
condense_compare [] = []

move_constants ((Inst_RR Mov r1 r2):rest) 
    | r1 == r2 =
    move_constants rest
move_constants ((Inst_RI Mov r1 (Const i)):(Inst_RR op r2 r3):rest) 
    | r1 == r3 =
    (Inst_RI op r2 (Const i)):(move_constants rest)
move_constants (x:xs) = x:move_constants xs
move_constants [] = []

condense_loads ((Inst_MemI Ld r r2 addr bf df):(Inst_RR Mov r3 r4):rest) 
    | r == r4 =
        (Inst_MemI Ld r3 r2 addr bf df):(condense_loads rest)
condense_loads ((Inst_Mem Ld r r2 bf):(Inst_RR Mov r3 r4):rest) 
    | r == r4 =
        (Inst_Mem Ld r3 r2 bf):(condense_loads rest)
condense_loads (x:xs) = x:condense_loads xs
condense_loads [] = []

--Does not work for calls to assembly functions, needs poplr instruction
tail_call_elimination :: [Instruction] -> [Instruction]
tail_call_elimination ((Inst_JmpI Call Al lbl):(Inst_RR Mov R7 R6):(Inst_R Pop R6):(Inst_R Pop R1):(Inst_Jmp Jmp Al R1):rest) =
    (Inst_RR Mov R7 R6):(Inst_R Pop R6):(Inst_JmpI Jmp Al lbl):(tail_call_elimination rest)
tail_call_elimination (x:xs) = x:tail_call_elimination xs
tail_call_elimination [] = []


reduce_instruction_size instrs = map (reduce_size) instrs

reduce_size :: Instruction -> Instruction
reduce_size (Inst_RI Cmp r (Const 0)) =
    Inst_RR Test r r
reduce_size (Inst_RI Mov r (Const 0)) = 
    Inst_RR Xor r r
reduce_size i = i
