module Eval where
import Parse
import Data.Bits

funcop ::  OP -> Int -> Int -> Int
funcop Plus = (+)
funcop Minus = (-)
funcop Mul = (*)
funcop Div = div
funcop Eq = apply (==)
funcop Ne = apply (/=)
funcop Gt = apply (>)
funcop Lt = apply (<)
funcop Le = apply (<=)
funcop Ge = apply (>=)
funcop Shl = shiftL
funcop Shr = shiftR
funcop And = (.&.)
funcop Or = (.|.)
funcop Xor = xor
apply f l r =
    let b = f l r
    in boolToInt b

boolToInt :: Bool -> Int
boolToInt b
        | b == True = 1
        | otherwise = 0

eval :: Tree -> Int
eval (Num x) = x
eval (Operator op left right) =
    let l = eval left
        r = eval right
        f = funcop op
    in
        f l r
