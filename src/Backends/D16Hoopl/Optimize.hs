{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Backends.D16Hoopl.Optimize (optimize) where
import Control.Monad
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Compiler.Hoopl
import Backends.D16Hoopl.ConstProp
import Backends.D16Hoopl.Simplify
import Backends.D16Hoopl.DeadCode
import Backends.D16Hoopl.SSA
import Backends.D16Hoopl.Kill
import Backends.D16Hoopl.Split

import Backends.D16Hoopl.RegisterAllocator


type ErrorM        = Either String
optimize ir = 
     runSimpleUniqueMonad $ runWithFuel 100 $ optimize_M ir

optimize_M :: [Proc] -> M [Proc]
optimize_M ir =  ( return ir >>= mapM optIr) 

optIr :: Proc -> CheckingFuelMonad SimpleUniqueMonad Proc 
optIr ir@(Proc {entry,body,args}) =
    -- Note: does not actually perform SSA conversion, but instead converts all vars to SVars -- 
    (return body)               >>=
    (ssaRun         entry args) >>= 
    (constPropRun   entry args) >>= 
    (deadCodeRun    entry args) >>= 
    (splitPassRun   entry args) >>=
    --(killCodeRun    entry args) >>=
    (allocate entry)    >>=
    \final -> 
    return $ ir {body = final}
    
        

runPass :: CheckpointMonad m => Label -> Graph Node C C -> [Var] -> 
            (Label -> Graph Node C C -> [Var] -> m (Graph Node C C,
           FactBase f, MaybeO x f)) -> m (Graph Node C C)
runPass e b a f = do
    (body,_,_) <- f e b a 
    return $ body    



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

