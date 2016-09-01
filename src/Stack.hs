module Stack where
import Instructions
import Data.List (sort)
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
type Stack = ([(StackEntry,Int)],Set Register)
data StackEntry = Undefined | Unknown Register | Known Int
    deriving (Show)

optimize :: [Instruction] -> [Instruction]
optimize i = i



pop :: State Stack (StackEntry, Int)
pop = do
    (stack,regs) <- get
    case stack of
        (x:xs) -> do
            put (xs,regs)
            return (x)
        [] ->
            return (Undefined,-1)
push :: (StackEntry, Int) -> State Stack ()
push a = do
    (stack,regs) <- get
    case a of
        ((Unknown reg),_)  ->
            put $ (a:stack,(Set.insert reg regs))
        _ ->
            put $ (a:stack,regs)



    return ()
clear :: State Stack ()
clear = put ([],Set.empty)

isValid :: Register -> State Stack (Bool)
isValid reg = do
    (_,regs) <- get
    if Set.member reg regs then
        return True
    else
        return False
invalidate :: Register -> State Stack ()
invalidate reg = do
    (stack, regs) <- get
    let regs' = Set.delete reg regs
    put (stack,regs')

run_stack_analysis :: [Instruction] -> [Instruction]
run_stack_analysis insts=
    let (instructions,indices) = fst $ runState (stack_analysis insts 0) ([],Set.empty)
        sorted = sort indices
    in stack_analysis_cleanup (instructions,sorted) 0
    --in trace (show sorted) (instructions)

stack_analysis :: [Instruction] -> Int -> State Stack ([Instruction],[Int])
stack_analysis ((Inst_I Push (Const v)):rest) x = do
    push ((Known v),x)
    (instructions,indices) <- stack_analysis rest (x+1)
    return ((Inst_I Push (Const v)):instructions,indices)
stack_analysis ((Inst_R Push reg):rest) x = do
    push ((Unknown reg),x)
    (instructions,indices) <- stack_analysis rest (x+1)
    return ((Inst_R Push reg):instructions,indices)

stack_analysis ((Inst_R Pop reg):rest) x = do
    (val,line) <- pop
    valid <- isValid reg
    invalidate reg
    case val of
        Undefined -> do
            (instructions,indices) <- stack_analysis rest (x+1)
            return ((Inst_R Pop reg):instructions,indices)
        Unknown reg2 -> do
            if reg == reg2 && valid && (line+3) >= x then do
                (instructions,indices) <- stack_analysis rest (x)
                return (instructions,line:indices)
            else do
                (instructions,indices) <- stack_analysis rest (x+1)
                return ((Inst_R Pop reg):instructions,indices)
        Known v -> do
            (instructions,indices) <- stack_analysis rest (x+1)
            return ((Inst_RI Mov reg (Const v)):instructions,line:indices)
    
stack_analysis ((Inst_Jmp Set cc reg):rest) x = do
    (instructions,indices) <- stack_analysis rest (x+1)
    return ((Inst_Jmp Set cc reg):instructions,indices)
stack_analysis ((Inst_Jmp op cc reg):rest) x = do
    clear
    (instructions,indices) <- stack_analysis rest (x+1)
    return ((Inst_Jmp op cc reg):instructions,indices)
stack_analysis ((Inst_JmpI op cc val):rest) x = do
    clear
    (instructions,indices) <- stack_analysis rest (x+1)
    return ((Inst_JmpI op cc val):instructions,indices)
stack_analysis (inst:rest) x= do
    invalidate_regs inst
    (instructions,indices) <- stack_analysis rest (x+1)
    return (inst:instructions, indices)

stack_analysis [] x = return ([],[])

invalidate_regs :: Instruction -> State Stack ()
invalidate_regs inst = do
    case inst of
        (Inst_R op reg) -> invalidate reg
        (Inst_RI op reg imm) -> invalidate reg
        (Inst_RR op rD rS) -> invalidate rD
        (Inst_Mem Ld rD rS _) -> invalidate rD
        (Inst_MemI Ld rD _ _ _ _ ) -> invalidate rD
        _ -> return ()
    return ()

stack_analysis_cleanup :: ([Instruction],[Int]) -> Int -> [Instruction]
stack_analysis_cleanup ((inst:rest),(entry:indices)) line =
    if entry == line then
         stack_analysis_cleanup (rest,indices) (line+1)
    else
        inst:(stack_analysis_cleanup (rest,(entry:indices)) (line+1))

stack_analysis_cleanup (instructions,[]) _ = instructions
stack_analysis_cleanup ([],_) _ = []
