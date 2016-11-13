{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, MultiParamTypeClasses #-}
module Backends.D16Hoopl.RegisterAllocator where
import qualified Data.Map as Map
import Control.Monad
import Compiler.Hoopl
import LinearScan.Hoopl
import LinearScan
import Instructions (Register(..))
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Debug.Trace(trace)

instance HooplNode Node where
    mkBranchNode l = (Branch l)
    mkLabelNode l = (Label l)



instance NodeAlloc Node Node where
    isCall _ = False
    
    isBranch (Branch _) = True
    isBranch (Cond _ _ _) = True
    isBranch (Call _ _ _ _) = True
    
    retargetBranch (Branch _) _ lbl = (Branch lbl)
    
    
    getReferences n = []
