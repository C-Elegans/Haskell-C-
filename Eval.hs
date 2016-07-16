module Eval where
import Parse 

funcop ::  OP -> Integer -> Integer -> Integer
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

apply f l r =
    let b = f l r
    in boolToInt b

boolToInt :: Bool -> Integer
boolToInt b
        | b == True = 1
        | otherwise = 0

eval :: Tree -> Integer
eval (Num x) = x
eval (Operator op left right) =
    let l = eval left
        r = eval right
        f = funcop op
    in
        f l r
