{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Backends.D16Hoopl.Optimize (optimize) where
import Control.Monad
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Compiler.Hoopl
import Backends.D16Hoopl.ConstProp
import Backends.D16Hoopl.Simplify
import Backends.D16Hoopl.DeadCode
import Backends.D16Hoopl.DeadStore
import Backends.D16Hoopl.SSA
import Backends.D16Hoopl.Kill
import Backends.D16Hoopl.Split
import Backends.D16Hoopl.CallAllocation
import Backends.D16Hoopl.RegisterAllocator
import Backends.D16Hoopl.NullPtrUB
import Backends.D16Hoopl.PostAlloc
import Backends.D16Hoopl.MemoryAnalysis
import Debug.Trace


type ErrorM        = Either String
optimize ir = 
     runSimpleUniqueMonad $ runWithFuel infiniteFuel $ optimizeM ir

optimizeM :: [Proc] -> M [Proc]
optimizeM = mapM optIr 

optIr :: Proc -> CheckingFuelMonad SimpleUniqueMonad Proc 
optIr ir@Proc {entry,body,args} =
    -- Note: does not actually perform SSA conversion, but instead converts all vars to SVars -- 
    (return body)               >>=
    (ssaRun         entry args) >>=
    (splitPassRun   entry args) >>=
    (nullPtrRun     entry args) >>=
    (constPropRun   entry args) >>=
    (memAnalRun     entry args) >>=
    (constPropRun   entry args) >>=
    (deadStoreRun   entry args) >>=
    (deadCodeRun    entry args) >>=
    (tracePass      ir) >>=
    (callAllocRun   entry args) >>=
    (allocate       entry     ) >>=
    (postAllocRun   entry args) >>=
    
    \final -> 
    return $ ir {body = final}
    
        
tracePass p body =
  trace(showProc p {body=body}) $ return body

runPass :: CheckpointMonad m => Label -> Graph Node C C -> [Var] -> 
            (Label -> Graph Node C C -> [Var] -> m (Graph Node C C,
           FactBase f, MaybeO x f)) -> m (Graph Node C C)
runPass e b a f = do
    (body,_,_) <- f e b a 
    return body    



ssaPass :: FuelMonad m => FwdPass m Node SSAFact
ssaPass = FwdPass {
    fp_lattice = ssaLattice,
    fp_transfer = assignSSAVar,
    fp_rewrite = ssaRewrite 
    }
ssaRun entry args body = 
    runPass entry body args (\ e b a -> analyzeAndRewriteFwd
        ssaPass (JustC e) b (mapSingleton e (initSSAFact a)) )

constPropPass :: FuelMonad m => FwdPass m Node ConstFact
constPropPass = FwdPass
  { fp_lattice  = constLattice
  , fp_transfer = varHasLit
  , fp_rewrite  = constProp `thenFwdRw` simplify }
constPropRun entry args body = 
    runPass entry body args (\ e b a -> analyzeAndRewriteFwd constPropPass (JustC e) b
       (mapSingleton e (initFact a)))
memAnalPass :: FuelMonad m => FwdPass m Node MemFact
memAnalPass = FwdPass
  { fp_lattice  = memLattice
  , fp_transfer = memTransfer
  , fp_rewrite  = memRewrite }
memAnalRun entry args body = 
    runPass entry body args (\ e b a -> analyzeAndRewriteFwd memAnalPass (JustC e) b
       mapEmpty )
deadStorePass :: FuelMonad m => BwdPass m Node LiveMem
deadStorePass = BwdPass
  { bp_lattice  = dsLattice
  , bp_transfer = liveStoreTransfer
  , bp_rewrite  = liveStoreRw}
deadStoreRun entry args body = 
    runPass entry body args (\ e b a -> analyzeAndRewriteBwd deadStorePass (JustC e) b
       mapEmpty )

deadCodePass :: FuelMonad m => BwdPass m Node Live
deadCodePass = BwdPass {
    bp_lattice = liveLattice,
    bp_transfer = liveness,
    bp_rewrite = deadCode
    }
deadCodeRun entry args body =
    runPass entry body args (\ e b _ -> analyzeAndRewriteBwd deadCodePass (JustC e) b mapEmpty)
killCodePass :: FuelMonad m => BwdPass m Node Kill    
killCodePass = BwdPass {
    bp_lattice = killLattice,
    bp_transfer = killed,
    bp_rewrite = killVars
    }
killCodeRun entry args body = 
    runPass entry body args (\ e b _ ->analyzeAndRewriteBwd 
        killCodePass (JustC e) b mapEmpty )

--regAllocatePass = FwdPass {
--    fp_lattice = regLattice,
--    fp_transfer = assignRegister,
--    fp_rewrite = rewriteRegister
--    }
splitPass :: FuelMonad m => FwdPass m Node SplitFact
splitPass = FwdPass {
    fp_lattice = splitLattice,
    fp_transfer = countNodes,
    fp_rewrite = splitExpr
    }
splitPassRun entry args body = 
    runPass entry body args (\e b _ -> analyzeAndRewriteFwd splitPass (JustC e) b mapEmpty)
callAllocPass :: FuelMonad m => FwdPass m Node CallFact
callAllocPass = FwdPass {
    fp_lattice = emptyLattice,
    fp_transfer = emptyTransfer,
    fp_rewrite = callAlloc
}
callAllocRun entry args body = 
    runPass entry body args (\e b _ -> analyzeAndRewriteFwd callAllocPass (JustC e) b mapEmpty)
nullPtrPass :: FuelMonad m => FwdPass m Node NullPtrFact
nullPtrPass = FwdPass {
    fp_lattice = nullLattice,
    fp_transfer = nullTransfer,
    fp_rewrite = nullRewrite
}

nullPtrRun entry args body = 
    runPass entry body args (\e b _ -> analyzeAndRewriteFwd nullPtrPass (JustC e) b mapEmpty)

postAllocPass :: FuelMonad m => FwdPass m Node CallFact
postAllocPass = FwdPass {
    fp_lattice = emptyLattice,
    fp_transfer = emptyTransfer,
    fp_rewrite = postAlloc
}
postAllocRun entry args body =
    runPass entry body args (\e b _ -> analyzeAndRewriteFwd postAllocPass (JustC e) b mapEmpty)
