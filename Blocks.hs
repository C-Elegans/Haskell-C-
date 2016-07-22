module Blocks where
import Parse (Tree(..), OP(..), Type(..),spaceTabs)
import Tree (getFunctions, getGlobals)

data BVar = BVar Type String
    deriving (Show)
data Block =
        BFunc Type String [BVar] [Block]
    |   BStatement [Tree] 
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

blockify tree = [BStatement [tree]]

--combines adjacent BStatements
reduce :: [Block] -> [Block]
reduce ((BStatement [first]):xs) =
    let rest = reduce xs
    in
        case rest of
            (BStatement second):blocks ->
                (BStatement (first:second)):blocks
            _ -> 
                (BStatement [first]):rest
reduce ((BFunc t str vars blk):blocks) =
    (BFunc t str vars (reduce blk)):(reduce blocks)
reduce ((BIf tree blk):blocks) =
    (BIf tree (reduce blk)):(reduce blocks)
reduce ((BIfElse tree left right):blocks) =
    (BIfElse tree (reduce left) (reduce right)):(reduce blocks)
reduce (x:xs) =
    x:(reduce xs)
reduce [] = []
    
print_tree_list level (x:xs) =
    do
        spaceTabs level
        putStrLn (show x)
        print_tree_list level xs
print_tree_list level [] = return ()

print_blocks :: Int -> [Block] -> IO ()
print_blocks level ((b:blocks)) = do
    spaceTabs level
    case b of
        (BFunc t str vars lst) -> do
            putStrLn $ "BFunc: " ++ str ++ (show vars)
            print_blocks (level+1) lst
        (BStatement tree) -> do
            putStrLn $ "BStatement: "
            print_tree_list (level+1) tree
        (BIf tree lst) -> do
            putStrLn $ "BIf: " ++ (show tree)
            print_blocks (level+1) lst
        (BIfElse cond left right) -> do
            putStrLn $ "BIfElse: " ++ (show cond)
            print_blocks (level+1) left
            spaceTabs level
            putStrLn $ "Else: "
            print_blocks (level+1) right
        (BVars vars) ->
            putStrLn $ "Vars" ++ (show vars)
        
    print_blocks level blocks
print_blocks _ [] = return ()

