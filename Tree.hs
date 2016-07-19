module Tree where
import Parse (Tree(..), OP(..), Type(..))
import Eval (funcop)
import Debug.Trace (trace)
post_apply :: (Tree -> Tree) -> Tree -> Tree
post_apply f (Num x) = f (Num x)
post_apply f (Operator o left right) = f (Operator o (post_apply f left) (post_apply f right))
post_apply f (Var str) = f (Var str)
post_apply f (List (x:xs)) = f (List ((post_apply f x):xs))
    
post_apply f (List []) = f (List [])
post_apply f (Return tree) = f (Return (post_apply f tree))
post_apply f (If cond tree) = f (If (post_apply f cond) (post_apply f tree))
post_apply f (IfElse cond left right) = f (IfElse (post_apply f cond) (post_apply f left) (post_apply f right))
post_apply f (Compound left right) = f (Compound (post_apply f left) (post_apply f right))
post_apply f (EmptyTree) = f (EmptyTree)
post_apply f (VarDec t str) = f (VarDec t str)
post_apply f (FuncDec t str left right) = (FuncDec t str (post_apply f left) (post_apply f right))
post_apply f (FCall str tree) = f (FCall str (post_apply f tree))

const_subexpr_simplification (Operator op (Num l) (Num r)) = Num ((funcop op) l r)
const_subexpr_simplification tree = tree

passes = [const_subexpr_simplification]

run_passes (pass:passes) tree =
    let tree' = post_apply pass tree
    in run_passes passes tree'

run_passes [] tree = tree
