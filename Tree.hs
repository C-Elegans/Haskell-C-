module Tree where
import Parse (Tree(..), OP(..), Type(..))
import Eval (funcop)
import Debug.Trace (trace)
post_apply :: (Tree -> Tree) -> Tree -> Tree
post_apply f (Num x) = f (Num x)
post_apply f (Operator o left right) = f (Operator o (f left) (f right))
post_apply f (Var str) = f (Var str)
post_apply f (List (x:xs)) = List ((f x):xs)
post_apply f (List []) = List []
post_apply f (Return tree) = f (Return (f tree))
post_apply f (If cond tree) = f (If (f cond) (f tree))
post_apply f (IfElse cond left right) = f (IfElse (f cond) (f left) (f right))
post_apply f (Compound left right) = f (Compound (f left) (f right))
post_apply f (EmptyTree) = f (EmptyTree)
post_apply f (VarDec t str) = f (VarDec t str)
post_apply f (FuncDec t str left right) = f (FuncDec t str (f left) (f right))
post_apply f (FCall str tree) = f (FCall str (f tree))

const_subexpr_simplification tree =
    trace (show tree) (
    case tree of
        (Operator o left right) ->
            trace "Operator" (
            case left of
                (Num l) -> 
                    trace "Left Num" (
                    case right of
                        (Num r) ->
                            Num ((funcop o) l r)
                        _ -> tree)
                _ -> tree)
        _ -> tree)
