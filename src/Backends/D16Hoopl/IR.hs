{-# LANGUAGE GADTs #-}
module Backends.D16Hoopl.IR where
import Backends.D16Hoopl.Expr
import qualified Parse
import Compiler.Hoopl
import Prelude hiding (succ)

type M = CheckingFuelMonad (SimpleUniqueMonad)

data Value = B Bool | I Integer deriving Eq

data Proc = Proc { name :: String, args :: [Var], entry :: Label, body :: Graph Node C C }

data Node e x where
  Label  :: Label  ->                               Node C O
  Assign :: Var    -> Expr    ->                    Node O O
  Store  :: Expr   -> Expr    ->                    Node O O
  Branch :: Label  ->                               Node O C
  Cond   :: Expr   -> Label   -> Label  ->          Node O C
  Call   :: [Var]  -> String  -> [Expr] -> Label -> Node O C
  Return :: [Expr] ->                               Node O C

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
  show (Assign v e)       = ind $ v ++ " = " ++ show e
  show (Store addr e)     = ind $ "m[" ++ show addr ++ "] = " ++ show e
  show (Branch lbl)       = ind $ "goto " ++ show lbl
  show (Cond e t f)       =
    ind $ "if " ++ show e ++ " then goto " ++ show t ++ " else goto " ++ show f
  show (Call ress f cargs succ) =
    ind $ tuple ress ++ " = " ++ f ++ tuple (map show cargs) ++ " goto " ++ show succ
  show (Return      rargs) = ind $ "ret " ++ tuple (map show rargs)

ind :: String -> String
ind x = "  " ++ x

instance Show Value where
  show (B b) = show b
  show (I i) = show i
