{-# LANGUAGE GADTs, TypeFamilies, ScopedTypeVariables,
    RankNTypes, FlexibleInstances, TypeSynonymInstances #-}

module Backends.D16Hoopl.Backend where
import Backends.D16Hoopl.ToIR
import Data.List
import Compiler.Hoopl
import Parse (Tree(GlobalDec, List))
import Instructions
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Optimize
import Backends.D16Hoopl.IRToInstruction
import Backends.D16Hoopl.Peephole
import Debug.Trace (trace)
runBackend :: Tree -> [(String,String)] -> String -> ([Instruction],String)
runBackend tree strings cleanfilename = 
    let ir = treeToIR tree
        ir' = trace (concat $ map showProc ir ) (optimize ir)
        
        insns = trace (concat $ map showProc ir' ) $ map (assemble) ir'
        insns' = map (runPeephole) insns
    in  (concat insns',"")








