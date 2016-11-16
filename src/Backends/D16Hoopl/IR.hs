{-# LANGUAGE GADTs #-}
module Backends.D16Hoopl.IR where
import Backends.D16Hoopl.Expr
import Control.Monad
import qualified Data.Map as M
import Control.Applicative as AP (Applicative(..))
import qualified Parse
import Compiler.Hoopl 
import Instructions (Register)
import Prelude hiding (succ)

type M = CheckingFuelMonad (SimpleUniqueMonad)

data Value = B Bool | I Integer deriving Eq

data Proc = Proc { name :: String, args :: [Var], entry :: Label, body :: Graph Node C C }

data Assignable = V Var | S SVar | R Register
instance Show Assignable where
    show (V v) = v
    show (S s) = show s
    show (R r) = show r

data Node e x where                                 
  Label  :: Label  ->                                       Node C O
  Assign :: Assignable    -> Expr    ->                     Node O O
  Store  :: Expr   -> Expr    ->                            Node O O
  Branch :: Label  ->                                       Node O C
  Cond   :: Expr   -> Label   -> Label  ->                  Node O C
  Call   :: [Assignable]  -> String  -> [Expr] -> Label ->  Node O C
  Return :: [Expr] ->                                       Node O C

instance NonLocal Node where
  entryLabel (Label l)      = l
  successors (Branch l)     = [l]
  successors (Cond _ t f)   = [t, f]
  successors (Call _ _ _ l) = [l]
  successors (Return _)     = []

--------------------------------------------------------------------------------
-- Prettyprinting
--------------------------------------------------------------------------------

showProc :: Proc -> String
showProc proc = name proc ++ tuple (args proc) ++ graph
  where
    graph  = " {\n" ++ showGraph show (body proc) ++ "}\n"

instance Show (Node e x) where
  show (Label lbl)        = show lbl ++ ":"
  show (Assign (V v) e)       = ind $ v ++ " = " ++ show e
  show (Assign (S s) e)       = ind $ (show s) ++ " = " ++ show e
  show (Assign (R r) e)       = ind $ (show r) ++ " = " ++ show e
  show (Store addr e)     = ind $ "m[" ++ show addr ++ "] = " ++ show e
  show (Branch lbl)       = ind $ "goto " ++ show lbl
  show (Cond e t f)       =
    ind $ "if " ++ show e ++ " then goto " ++ show t ++ " else goto " ++ show f
  show (Call ress f cargs succ) =
    ind $ tuple (map show ress) ++ " = " ++ f ++ tuple (map show cargs) ++ " goto " ++ show succ
  show (Return      rargs) = ind $ "ret " ++ tuple (map show rargs)
  --show (Nop) = "nop"
ind :: String -> String
ind x = "  " ++ x

instance Show Value where
  show (B b) = show b
  show (I i) = show i



--------------------------------------------------------------------------------
-- The LabelMapM monad
--------------------------------------------------------------------------------

type IdLabelMap = M.Map String Label
data LabelMapM a = LabelMapM (IdLabelMap -> M (IdLabelMap, a))

instance Monad LabelMapM where
  return = AP.pure
  LabelMapM f1 >>= k = LabelMapM (\m -> do (m', x) <- f1 m
                                           let (LabelMapM f2) = k x
                                           f2 m')

instance Functor LabelMapM where
  fmap = liftM

instance Applicative LabelMapM where
  pure x = LabelMapM (\m -> return (m, x))
  (<*>) = ap

labelFor l = LabelMapM f
  where f m = case M.lookup l m of
                Just l' -> return (m, l')
                Nothing -> do l' <- freshLabel
                              let m' = M.insert l l' m
                              return (m', l')


uniqueLabel = LabelMapM f
  where f m = do l' <- freshLabel; return(m,l')

getBody graph = LabelMapM f
  where f m = return (m, graph)

run (LabelMapM f) = f M.empty >>= return . snd
