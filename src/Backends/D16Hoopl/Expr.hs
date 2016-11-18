{-# LANGUAGE DeriveDataTypeable #-}
module Backends.D16Hoopl.Expr where
import Data.Data
import qualified Parse
import Instructions (Register)

data Expr = Lit   Lit
          | Var   Var
          | SVar  SVar
          | Str Var
          | Reg   Register
          | Call  String [Expr]
          | Load  Expr
          | Binop BinOp Expr Expr 
          | Unop  UnOp Expr
        deriving (Eq, Typeable, Data)
          
data BinOp = Add | Sub | Mul | Div | Eq | Ne | Lt | Gt | Lte | Gte | 
    Shl | Shr | And | Or | Xor | Sar
    deriving (Eq,Data)
data UnOp = Not | Neg | Addr
    deriving (Eq,Data)

data Lit = Bool Bool | Int Int deriving (Eq,Data)
type Var = String
data S_Flags = S_None | S_Kill deriving (Eq,Ord,Data)
data SVar = Svar Var Int S_Flags deriving (Data)

svToString :: SVar -> String
svToString (Svar n i _) = n ++ (show i)
instance Eq SVar where
    (Svar name i _) == (Svar name2 i2 _) = name == name2 && i == i2
instance Ord SVar where
    compare (Svar name _ _) (Svar name2 _ _)
        | name /= name2 = compare name name2
    compare (Svar name i _) (Svar name2 i2 _) 
        = compare i i2

instance Show Expr where
  show (Lit   i) = show i
  show (Var   v) = v
  show (SVar  s) = show s
  show (Reg   r) = show r
  show (Load  e) = "m[" ++ show e ++ "]"
  show (Str   s) = "Str " ++ s
  show (Call name s) = 
        name ++ tuple (map show s)
  show (Binop b e1 e2) = sub e1 ++ " " ++ show b ++ " " ++ sub e2
    where sub e@(Binop _ _ _) = tuple [show e]
          sub e = show e
  show (Unop u l) = (show u) ++ (show l)

instance Show Lit where
  show (Int  i) = show i
  show (Bool b) = show b
instance Show S_Flags where
  show (S_None) = ""
  show (S_Kill) = "(kill)"
instance Show SVar where
  show (Svar v i f) = v ++ "_" ++ (show i) ++ " " ++ (show f) 
instance Show BinOp where
  show Add  = "+"
  show Sub  = "-"
  show Mul  = "*"
  show Div  = "/"
  show Eq   = "=="
  show Ne   = "/="
  show Gt   = ">"
  show Lt   = "<"
  show Gte  = ">="
  show Lte  = "<="
  show Shl  = "<<"
  show Shr  = ">>"
  show And  = "&"
  show Or   = "|"
  show Xor  = "^"
  show Sar  = ">>"
instance Show UnOp where
  show Not = "~"
  show Neg = "-"
  show Addr = "&"

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
opToBinOp Parse.Shl = Shl
opToBinOp Parse.Shr = Shr
opToBinOp Parse.And = And 
opToBinOp Parse.Or  = Or
opToBinOp Parse.Xor = Xor  
opToBinOp Parse.Sar = Sar

opToUnOp :: Parse.OP -> UnOp
opToUnOp Parse.Not = Not
opToUnOp Parse.Neg = Neg

