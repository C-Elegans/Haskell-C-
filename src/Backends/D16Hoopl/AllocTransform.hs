{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, NamedFieldPuns #-}
module Backends.D16Hoopl.AllocTransform (runAllocTransform) where
import Control.Monad
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.OptSupport
import Instructions (Register(..))
import Compiler.Hoopl
import Data.Maybe
import Prelude hiding ((<*>))
import Debug.Trace (trace)
import qualified Data.Map as Map

data Alias = NoAlias | MayAlias | Aliases [Var]
  deriving (Show, Eq)

amax :: Alias -> Alias -> Alias
amax (Aliases x) (Aliases y) = Aliases (x++y)
amax (MayAlias) _ = MayAlias
amax _ (MayAlias) = MayAlias
amax NoAlias x = x

data AllocFact = AllocFact {
  hmap :: Map.Map Var Int,
  alias :: Map.Map Var Alias,
  sp  :: Int,
  tmp :: Int
  }
  deriving (Show, Eq)

emptyfact = AllocFact{ hmap=Map.empty, alias=Map.empty, sp=0, tmp=0}
lattice :: DataflowLattice AllocFact
lattice = DataflowLattice {
    fact_name = "Alloc Map",
    fact_bot = emptyfact,
    fact_join = add }
    where
        add _ (OldFact old) (NewFact new) =
            let m = Map.unionWith max (hmap old) (hmap new)
                --a = Map.unionWith amax (alias old) (alias new)
                a = Map.union (alias old) (alias new)
                s = max (sp old) (sp new)
                t = max (tmp old) (tmp new)
                newfact =  AllocFact {hmap = m, sp = s, tmp=t, alias=a}
                changeFlag = changeIf (newfact /= old || newfact /= new)
            in (changeFlag,newfact)

pass :: FuelMonad m => FwdPass m Node AllocFact
pass = FwdPass {
    fp_lattice = lattice,
    fp_transfer = transfer,
    fp_rewrite = rewrite 
    }


rewrite :: forall m . FuelMonad m => FwdRewrite m Node AllocFact
rewrite = deepFwdRw rw
  where
    rw :: Node e x -> AllocFact -> m (Maybe (Graph Node e x))
    --rw n f | trace (show n ++ ": " ++  show f) False = return $ Nothing
    --rw (Assign _ (Alloca n)) _ = return $ Just $ insnToG $ None $ Alloca n
    rw n@(Assign (V v) e) f =
      let aliases = aliasanalyze Map.empty e
      in if aliases == Map.empty then
           return Nothing
         else
           let (spills, f') = genSpills f (Map.toList aliases)
               n' = fromMaybe emptyGraph $ liftM insnToG $ (mapEN . mapEE) (cvt f') n
           in return $ Just $ spills <*> n'
    rw node f = return $ liftM insnToG $ (mapEN . mapEE ) (cvt f) node
    rw node f = return $ Nothing
    genSpills :: AllocFact -> [(String, Alias)] -> (Graph Node O O, AllocFact)
    genSpills f ((var,flag):rest) =
      case Map.lookup var (hmap f) of
        Just i -> genSpills f rest
        Nothing ->
          let alloca = insnToG $ Assign (V var) (Alloca 2)
              spill = insnToG $ Store (Binop Sub (Reg R6) (Lit (Int ((sp f)+2)))) (Var var) Word
              mp = Map.insert var ((sp f) + 2) (hmap f)
              f' = f {sp = (sp f) + 2, hmap=mp}
              (spills, f'') = genSpills f' rest
          in ( spill <*> alloca <*>spills, f'')
    genSpills f [] = (emptyGraph, f)
        

      
    cvt f (Var str) = 
      case Map.lookup str (hmap f) of
        Just i -> Just $ Load (Binop Sub (Reg R6) (Lit (Int i))) Word
        Nothing -> Nothing
    cvt f e1@(Unop Addr (Load e _)) = Just $ e 
      
    cvt _ e = Nothing
    
transfer :: FwdTransfer Node AllocFact
transfer = mkFTransfer ft
  where
    ft :: Node e x -> AllocFact -> Fact x AllocFact
    ft n f | trace ("ft: " ++show n ++ " " ++ show f) False = undefined
    ft (Label _)            f = f
    ft (Assign (V v) (Alloca i)) f = trace ("Assign alloca") $
        let s = (sp f) + 2
            hm = Map.insert v s (hmap f)
        in f {hmap=hm, sp=s}
    ft (Assign (V v) e)     f =
      let amap = aliasanalyze (alias f) e
      in case Map.lookup v (hmap f) of
        Just i -> f {alias=amap}
        Nothing ->
          case Map.lookup v amap of
            Just MayAlias ->
                let  s = (sp f) + 2
                     m = Map.insert v s (hmap f)
                     a = Map.insert v NoAlias amap
                in f{hmap=m, sp=s, alias=a}
            _ -> f{alias = amap}
    ft (Assign e (Unop Addr (Var v))) f =
        let a = case Map.lookup v (alias f) of
                  Nothing -> MayAlias
                  Just x -> amax x (Aliases [v])
        in f {alias= Map.insert v a (alias f)}

          
    ft (Assign e _)         f = f
    
    ft (Store _ _ _)          f = f
    ft (Branch l)           f = mapSingleton l f
    ft (Cond _ tl fl)       f =
        mkFactBase lattice [(tl,f), (fl,f)]
    ft (None _)             f = f
    ft (Return _)           _ = mapEmpty
aliasanalyze :: Map.Map Var Alias -> Expr -> Map.Map Var Alias
aliasanalyze a (Unop Addr (Var v)) =
    Map.insert v MayAlias a
aliasanalyze a _ = a

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
  --let dp = debugFwdTransfers trace show (\x y -> True) pass
  let dp = pass
  in runPass entry body args (\ e b a -> analyzeAndRewriteFwd
        dp (JustC e) b (mapSingleton e emptyfact)) 
