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
import Backends.D16Hoopl.AllocTransform
import Debug.Trace (trace)
runBackend :: Tree -> [(String,String)] -> String -> ([Instruction],String)
runBackend tree strings cleanfilename = 
    let ir = treeToIR tree
        ir' = trace (concat $ map showProc ir) $ runAllocTransform ir
        
        ir'' = trace (concat $ map showProc ir' ) (optimize ir')
        
        insns = trace ("after optimization:\n" ++ (concat $ map showProc ir'')) $ map (assemble) ir''
        insns' = map (runPeephole) insns
        assembledStrings = trace (show strings) $ assemble_strings strings cleanfilename
    in  (concat insns',assembledStrings)
    -- in (ir'',  [])


assemble_strings :: [(String,String)] -> String -> String
assemble_strings (s:strs) filename =
    let (label,string) = s
        assembledString = label ++ ":\n    "++".asciz \"" ++ (escape string) ++"\"\n"
    in assembledString ++ (assemble_strings strs filename)
assemble_strings [] _ = []


escape :: String -> String
escape ('\n':cs) = '\\':'n':(escape cs)
escape ('\0':cs) = '\\':'0':(escape cs)
escape ('\r':cs) = '\\':'r':(escape cs)
escape (c:cs) = c:(escape cs)
escape [] = []





