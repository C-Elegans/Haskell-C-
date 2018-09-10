{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, NamedFieldPuns #-}
module Backends.D16Hoopl.AllocTransform (runAllocTransform) where
import Control.Monad
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.OptSupport
import Instructions (Register(..))
import Compiler.Hoopl
import Prelude hiding ((<*>))
import Debug.Trace (trace)
import qualified Data.Map as Map

data AllocFact = AllocFact {
  hmap :: Map.Map Var Int,
  sp  :: Int,
  tmp :: Int
  }
  deriving (Show)

emptyfact = AllocFact{ hmap=Map.empty, sp=0, tmp=0}
lattice :: DataflowLattice AllocFact
lattice = DataflowLattice {
    fact_name = "Alloc Map",
    fact_bot = emptyfact,
    fact_join = add }
    where
        add _ (OldFact old) (NewFact new) =
            let m = Map.unionWith max (hmap old) (hmap new)
                changeFlag = changeIf  False
                s = max (sp old) (sp new)
                t = max (tmp old) (tmp new)
            in (changeFlag, AllocFact {hmap = m, sp = s, tmp=t})

pass :: FuelMonad m => FwdPass m Node AllocFact
pass = FwdPass {
    fp_lattice = lattice,
    fp_transfer = transfer,
    fp_rewrite = rewrite 
    }


rewrite :: forall m . FuelMonad m => FwdRewrite m Node AllocFact
rewrite = mkFRewrite rw
  where
    rw :: Node e x -> AllocFact -> m (Maybe (Graph Node e x))
    rw (Assign (V v) a) f =
      case Map.lookup v (hmap f) of
        Just i ->
            return $ Just $ insnToG $
            Store (Binop Sub (Reg R6) (Lit (Int i))) a Word
        Nothing ->
          let n = Assign (V v) $ Alloca 2
              s = (sp f) + 2
              n2 = Store (Binop Sub (Reg R6) (Lit (Int s))) a Word
          in return $ Just $ insnToG n <*> insnToG n2
      
    rw node f = return $ liftM insnToG $ (mapEN . mapEE ) (cvt f) node
    cvt f (Var str) = 
      case Map.lookup str (hmap f) of
        Just i -> Just $ Load (Binop Sub (Reg R6) (Lit (Int i))) Word
        Nothing -> undefined
    cvt _ e = Nothing
    
transfer :: FwdTransfer Node AllocFact
transfer = mkFTransfer ft
  where
    ft :: Node e x -> AllocFact -> Fact x AllocFact
    ft (Label _)            f = f
    ft (Assign (V v) _)     f =
      case Map.lookup v (hmap f) of
        Just i -> f
        Nothing ->
          let  s = (sp f) + 2
               m = Map.insert v s (hmap f)
          in f{hmap=m, sp=s}
          
    ft (Assign e _)         f = f
    
    ft (Store _ _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase lattice [(tl,f), (fl,f)]
    ft (None _)             f = f
    ft (Return _)           _ = mapEmpty

runAllocTransform :: [Proc] -> [Proc]
runAllocTransform p =
  runSimpleUniqueMonad $ runWithFuel infiniteFuel $ mapM allocpass p

allocpass :: Proc -> CheckingFuelMonad SimpleUniqueMonad Proc
allocpass p =
  (return (body p)) >>=
  (runp (entry p) (args p)) >>=
  \(final, fb) -> trace (show fb) $ return $ p {body=final}
          



runPass :: (CheckpointMonad m, Show f) => Label -> Graph Node C C -> [Var] -> 
            (Label -> Graph Node C C -> [Var] -> m (Graph Node C C,
           FactBase f, MaybeO x f)) -> m (Graph Node C C, FactBase f)
runPass e b a f = do
    (body,fb,m) <- f e b a 
    return $ trace (show fb) (body, fb)



runp entry args body = 
    runPass entry body args (\ e b a -> analyzeAndRewriteFwd
        pass (JustC e) b (mapSingleton e emptyfact)) 
