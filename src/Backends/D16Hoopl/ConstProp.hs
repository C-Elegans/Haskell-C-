{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Backends.D16Hoopl.ConstProp where
import qualified Data.Map as Map
import Compiler.Hoopl
import Data.Maybe (fromMaybe)
import Instructions (Register(..))
import Backends.D16Hoopl.Expr
import Backends.D16Hoopl.IR
import Backends.D16Hoopl.OptSupport
import Debug.Trace

{-
 -This pass performs constant propogation among SVars and is run in tandem with the 
 -pass in Simplify.hs
 -}
data PropData = L Lit
  | Disp Register Int
  | Mem Register Int
  deriving (Show, Eq)
type ConstFact = Map.Map SVar (WithTop PropData)
constLattice :: DataflowLattice ConstFact
constLattice = DataflowLattice {
    fact_name = "Constant Value",
    fact_bot = Map.empty,
    fact_join = joinMaps (extendJoinDomain constFactAdd) }
    where
        constFactAdd _ (OldFact old) (NewFact new) =
            if new == old then  (NoChange, PElem new)
            else                (SomeChange, Top)    
            
initFact :: [Var] -> ConstFact
initFact vars = Map.fromList [(Svar v 0 S_None,Top) | v <- vars]

varHasLit :: FwdTransfer Node ConstFact
varHasLit = mkFTransfer ft
  where
    ft :: Node e x -> ConstFact -> Fact x ConstFact
    ft (Label _)                        f = f
    ft (Assign (S x) (Binop Sub (Reg r) (Lit (Int i)))) f = trace ("Displacement")
      Map.insert x (PElem (Disp r (0-i))) f
    ft (Assign (S x) (Load (Binop Add (Reg r) (Lit (Int i))) bf)) f =
      Map.insert x (PElem (Mem r i)) f
    ft (Assign (S x) (Lit k))           f = Map.insert x (PElem (L k)) f
    ft (Assign (S x) _)                 f = Map.insert x Top f
    ft (Assign (R _) _)                 f = f
    -- ft (Store (Binop Add (Reg r) (Lit (Int i))) e bf) f =
    --   let pairs = filter (\(x,y) -> y == (PElem (Mem r i))) (Map.toList f)
    --       keys = map (\(x,y) -> x) pairs
    --       mp = foldl (\mp k -> Map.delete k mp) f keys
    --   in case e of
    --     (SVar s) ->
    --       case Map.lookup s f of
    --         Just (PElem i) -> mp
    --         _ -> Map.insert s (PElem (Mem r i)) mp
    --     _ -> mp
    ft (Store _ _ _)                      f = f
    ft (Branch l)                       f = mapSingleton l f
    ft (Cond (Binop Eq (SVar x) (Lit l)) tl fl) f =
      mkFactBase constLattice
      [(tl, Map.insert x (PElem (L l)) f), (fl, f)]
    ft (Cond (SVar x) tl fl)            f =
        mkFactBase constLattice
            [(tl, f),
             (fl, Map.insert x (PElem (L (Int 0)))f)]
    ft (Cond _ tl fl)                   f =
        mkFactBase constLattice [(tl,f), (fl,f)]
    ft (None (Call _ _)) f =
      let pairs = filter pred (Map.toList f)
          keys = map (\(x,y) -> x) pairs
          mp = foldl (\mp k -> Map.delete k mp) f keys
      in mp
      where
        pred (_, (PElem (Mem _ _))) = True
        pred _ = False
    ft (None _)                         f = f
    ft (Return _)                       _ = mapEmpty
    ft n _ = error  $ "No ft defined for " ++ show n




constProp :: forall m . FuelMonad m => FwdRewrite m Node ConstFact
constProp = mkFRewrite cp
  where
    cp :: Node e x -> ConstFact -> m (Maybe (Graph Node e x))
    --cp n f | trace (show n ++ "  " ++ show f) False = undefined
    cp n@(Store loc val bf) f = 
      let expr = mapEE (lookup f) loc
          res =  return $ fmap insnToG $ Just (Store (fromMaybe loc expr) val bf)
      in case loc of
        SVar sv ->
          case Map.lookup sv f of
            Just (PElem (Disp r i)) -> return $ Just $ insnToG $ Store (Binop Add (Reg r) (Lit (Int i))) val bf
            _ -> res
        _ -> res

        
    cp node f =
        return $ fmap insnToG $ mapVN (lookup f) node
    mapVN :: (Expr -> Maybe Expr) -> MaybeChange (Node e x)
    mapVN = mapEN . mapEE 
    
    lookup :: ConstFact -> Expr -> Maybe Expr
    lookup f (Load (Binop Add (Reg r) (Lit (Int i))) _) = trace ("replacing read") $
      let pairs = filter (\(x,y) -> y == (PElem (Mem r i))) (Map.toList f)
          keys = map (\(x,y) -> x) pairs
      in if keys /= [] then
        Just $ SVar (head keys)
      else
        Nothing
    lookup f (Load (SVar x) ms) =

      case Map.lookup x f of
        Just (PElem (Disp r i)) -> Just $ Load (Binop Add (Reg r) (Lit (Int i))) ms
        _ -> Nothing
    lookup f (SVar x) = case Map.lookup x f of
        Just (PElem (L v)) -> Just $ Lit v
        _              -> Nothing
    lookup f _ = Nothing
