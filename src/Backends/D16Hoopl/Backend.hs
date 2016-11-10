module Backends.D16Hoopl.Backend where
import Backends.D16Hoopl.ToIR
import Backends.D16Hoopl.IRToInstruction
import Parse (Tree(GlobalDec, List))
import Instructions
import Tree (getFunctions, getGlobals )
runBackend :: Tree -> [(String,String)] -> String -> ([Instruction],String)
runBackend tree strings cleanfilename = 
    let ir = treeToIR tree
        insns = assemble ir
    in  (insns,"")
