{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Backends.D16Hoopl.Optimize (optimize) where

import Backends.D16Hoopl.IR
import Compiler.Hoopl
import Backends.D16Hoopl.ConstProp
import Backends.D16Hoopl.Simplify
import Backends.D16Hoopl.DeadCode
import Backends.D16Hoopl.SSA


type ErrorM        = Either String
optimize ir = 
     runSimpleUniqueMonad $ runWithFuel 9999 $ optimize_M ir

optimize_M :: [Proc] -> M [Proc]
optimize_M ir =  ( return ir >>= mapM optIr) 

   
optIr ir@(Proc {entry,body,args}) = do
    (body', _, _ ) <- analyzeAndRewriteFwd ssaPass (JustC entry) body 
        (mapSingleton entry (initSSAFact args))
    (body'', _, _ ) <- analyzeAndRewriteFwd constPropPass (JustC entry) body' 
       (mapSingleton entry (initFact args))
    (body''', _, _) <- analyzeAndRewriteBwd deadCodePass (JustC entry) body'' mapEmpty
    return $ ir {body = body'}

ssaPass :: FuelMonad m => FwdPass m Node SSAFact
ssaPass = FwdPass {
    fp_lattice = ssaLattice,
    fp_transfer = assignSSAVar,
    fp_rewrite = ssaRewrite 
    }

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
