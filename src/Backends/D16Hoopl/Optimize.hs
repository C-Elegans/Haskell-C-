{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Backends.D16Hoopl.Optimize (optimize) where

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

   
optIr ir@(Proc {entry,body,args}) = do
    -- Note: does not actually perform SSA conversion, but instead converts all vars to SVars -- 
    body' <- runPass entry body args (\ e b a -> analyzeAndRewriteFwd 
        ssaPass (JustC e) b (mapSingleton e (initSSAFact a)) )
    body''  <- runPass entry body' args (\ e b a -> analyzeAndRewriteFwd constPropPass (JustC e) b
       (mapSingleton e (initFact a)))
    body''' <- runPass entry body'' args (\ e b _ -> analyzeAndRewriteBwd deadCodePass (JustC e) b mapEmpty)
    body'''' <- runPass entry body''' args (\e b _ -> analyzeAndRewriteFwd splitPass (JustC e) b mapEmpty)
    body''''' <- runPass entry body'''' args (\ e b _ ->analyzeAndRewriteBwd 
        killCodePass (JustC e) b mapEmpty )
    --(body''''',_,_) <- analyzeAndRewriteFwd regAllocatePass (JustC entry) body'''' 
    --    (mapSingleton entry (initRegs args))
    return $ ir {body = body'''''}

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
killCodePass = BwdPass {
    bp_lattice = killLattice,
    bp_transfer = killed,
    bp_rewrite = killVars
    }

--regAllocatePass = FwdPass {
--    fp_lattice = regLattice,
--    fp_transfer = assignRegister,
--    fp_rewrite = rewriteRegister
--    }
splitPass = FwdPass {
    fp_lattice = splitLattice,
    fp_transfer = countNodes,
    fp_rewrite = splitExpr
    }


