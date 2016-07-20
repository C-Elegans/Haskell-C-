module Blocks where
import Parse (Tree(..), OP(..), Type(..))
import Tree (getFunctions, getGlobals)

data BVar = BVar Type String
    deriving (Show)
data Block =
        BFunc Type String [BVar] [Block]
    |   BStatement Tree 
    |   BIf Tree [Block]
    |   BIfElse Tree [Block] [Block]
    |   BVars [BVar]
    deriving (Show)
vars :: Tree -> [BVar]
vars (List (x:xs)) = 
    case x of
        (VarDec t str) ->
            (BVar t str):(vars (List xs))
        _ ->
            []
vars (List []) = []

blockify :: Tree -> [Block]
blockify (FuncDec t str pars stmts) =
    let vs = vars pars
    in [BFunc t str vs (blockify stmts)]
blockify (Compound decls stmts) =
    let vs = vars decls
    in (BVars vs):(blockify stmts)
blockify (If cond tree) =
    [BIf cond (blockify tree)]
blockify (IfElse cond left right) =
    [BIfElse cond (blockify left) (blockify right)]
blockify (List (x:xs)) =
    (head (blockify x)):(blockify (List xs))
blockify (List []) = []

blockify tree = [BStatement tree]


    
