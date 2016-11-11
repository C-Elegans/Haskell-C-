
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Backends.D16Hoopl.Backend where
import Backends.D16Hoopl.ToIR
import Compiler.Hoopl
import Backends.D16Hoopl.IRToInstruction
import Parse (Tree(GlobalDec, List))
import Instructions
import Backends.D16Hoopl.IR

import Backends.D16Hoopl.ConstProp
import Backends.D16Hoopl.Simplify
import Backends.D16Hoopl.DeadCode
import Debug.Trace (trace)
runBackend :: Tree -> [(String,String)] -> String -> ([Instruction],String)
runBackend tree strings cleanfilename = 
    let ir = treeToIR tree
        ir' = trace (concat $ map showProc ir ) 
            (runSimpleUniqueMonad $ runWithFuel 9999 $ optimize ir)
        insns = trace (concat $ map showProc ir' ) ([])
        
    in  (concat insns,"")


type ErrorM        = Either String
optimize :: [Proc] -> M [Proc]
optimize ir =  ( return ir >>= mapM optIr) 

   
optIr ir@(Proc {entry,body,args}) = do
    (body', _, _ ) <- analyzeAndRewriteFwd constPropPass (JustC entry) body 
        (mapSingleton entry (initFact args))
    (body'', _, _) <- analyzeAndRewriteBwd deadCodePass (JustC entry) body' mapEmpty
    return $ ir {body = body''}

constPropPass :: FuelMonad m => FwdPass m Node ConstFact
constPropPass = FwdPass
  { fp_lattice  = constLattice
  , fp_transfer = varHasLit
  , fp_rewrite  = constProp `thenFwdRw` simplify }
deadCodePass :: FuelMonad m => BwdPass m Node Live
deadCodePass = BwdPass {
    bp_lattice = liveLattice,
    bp_transfer = liveness,
    bp_rewrite = deadCode
    }
