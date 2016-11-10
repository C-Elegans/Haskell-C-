module Backends.D16Hoopl.Expr where
import qualified Parse

data Expr = Lit   Lit
          | Var   Var
          | Load  Expr
          | Binop BinOp Expr Expr deriving (Eq)
          
data BinOp = Add | Sub | Mul | Div | Eq | Ne | Lt | Gt | Lte | Gte deriving Eq

data Lit = Bool Bool | Int Integer deriving Eq
type Var = String

instance Show Expr where
  show (Lit   i) = show i
  show (Var   v) = v
  show (Load  e) = "m[" ++ show e ++ "]"
  show (Binop b e1 e2) = sub e1 ++ " " ++ show b ++ " " ++ sub e2
    where sub e@(Binop _ _ _) = tuple [show e]
          sub e = show e

instance Show Lit where
  show (Int  i) = show i
  show (Bool b) = show b

instance Show BinOp where
  show Add  = "+"
  show Sub  = "-"
  show Mul  = "*"
  show Div  = "/"
  show Eq   = "="
  show Ne   = "/="
  show Gt   = ">"
  show Lt   = "<"
  show Gte  = ">="
  show Lte  = "<="

tuple :: [String] -> String
tuple []     = "()"
tuple [a]    = "(" ++ a ++ ")"
tuple (a:as) = "(" ++ a ++ concat (map ((++) ", ") as) ++ ")"

opToBinOp :: Parse.OP -> BinOp
opToBinOp Parse.Plus = Add
opToBinOp Parse.Minus = Sub
opToBinOp Parse.Mul = Mul
opToBinOp Parse.Div = Div
opToBinOp Parse.Eq = Eq
opToBinOp Parse.Ne = Ne
opToBinOp Parse.Gt = Gt
opToBinOp Parse.Lt = Lt
opToBinOp Parse.Ge = Gte
opToBinOp Parse.Le = Lte
