module Backends.D16Hoopl.Backend where
import Backends.D16Hoopl.ToIR
import Compiler.Hoopl
import Backends.D16Hoopl.IRToInstruction
import Parse (Tree(GlobalDec, List))
import Instructions
import Tree (getFunctions, getGlobals )
import Debug.Trace (trace)
runBackend :: Tree -> [(String,String)] -> String -> ([Instruction],String)
runBackend tree strings cleanfilename = 
    let ir = treeToIR tree
        
        insns = trace (concat $ map (showGraph show ) ir ) (map (assemble) ir)
        
    in  (concat insns,"")
