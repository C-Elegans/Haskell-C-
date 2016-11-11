
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Backends.D16Hoopl.Backend where
import Backends.D16Hoopl.ToIR
import Compiler.Hoopl
import Backends.D16Hoopl.IRToInstruction
import Parse (Tree(GlobalDec, List))
import Instructions
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.ConstProp
import Tree (getFunctions, getGlobals )
import Debug.Trace (trace)
runBackend :: Tree -> [(String,String)] -> String -> ([Instruction],String)
runBackend tree strings cleanfilename = 
    let ir = treeToIR tree
        ir' = runSimpleUniqueMonad $ runWithFuel 999 $ optimize ir
        insns = trace (concat $ map showProc ir' ) ([])
        
    in  (concat insns,"")


type ErrorM        = Either String
optimize :: [Proc] -> M [Proc]
optimize ir =  ( return ir >>= mapM optIr) 

   
optIr ir@(Proc {entry,body,args}) = do
    (body', _, _ ) <- analyzeAndRewriteFwd constPropPass (JustC entry) body 
        (mapSingleton entry (initFact args))
    return $ ir {body = body'}
