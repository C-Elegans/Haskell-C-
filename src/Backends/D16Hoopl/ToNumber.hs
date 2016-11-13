{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.ToNumber where
import qualified Data.Map as Map
import Control.Monad
import Compiler.Hoopl
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Debug.Trace (trace)
import Prelude hiding ((<*>))


type NumberFact = (Map.Map SVar Int, Int)
numberLattice :: DataflowLattice NumberFact
numberLattice = DataflowLattice {
    fact_name = "Temp counter",
    fact_bot = (Map.empty,0),
    fact_join = add }
    where
        add _ (OldFact (om,oi)) (NewFact (nm,ni)) =
            let i = (Map.unionWith (max) om nm   ,max oi ni) 
                changeFlag = changeIf False
            in (changeFlag, i)
            

updateCount :: FwdTransfer Node NumberFact
updateCount = mkFTransfer ft
  where
    ft :: Node e x -> NumberFact -> Fact x NumberFact
    ft (Label _)            f = f
    ft (Assign (S v) _)         (m,i) = (Map.insert v i m, i+1)
    
    ft (Store _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase numberLattice [(tl,f ), (fl,f)]
    ft (Call _ _ _ bid)    f = 
        mapSingleton bid f
    ft (Return _)           _ = mapEmpty
        
    


numberNodes :: forall m . FuelMonad m => FwdRewrite m Node NumberFact
numberNodes = mkFRewrite split
  where
    split (Assign (S s) e) f = trace (show f) $
        let newVar = case lookup f s of
                        Just (SVar sv) -> sv
                        Nothing -> s
            expr = case (mapEE . mapSVE) (lookup f) e of
                    Just ex -> ex
                    Nothing -> e
            node = Just (Assign (S newVar) expr)
            
        in  return $ liftM insnToG $ node
    split n f = return $ liftM insnToG $ mapSVN (lookup f) n
    
    mapSVN = mapEN . mapEE . mapSVE
    
    lookup :: NumberFact -> SVar -> Maybe Expr
    lookup (m,_) v@(Svar name index flags) = case Map.lookup v m of
        Just i -> Just $ SVar (Svar "" i flags)
        _ -> Nothing
