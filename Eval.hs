module Eval where
import Parse 

funcop :: Integral a => OP -> a -> a -> a
funcop Plus = (+)
funcop Minus = (-)
funcop Mul = (*)
funcop Div = div

eval :: Tree -> Integer
eval (Num x) = x
eval (Operator op left right) =
    let l = eval left
        r = eval right
        f = funcop op
    in
        f l r
