module Main where
import Parse
import Eval
import Tree
import Blocks
import SSA
import System.Environment
import System.IO
import Debug.Trace (trace)
main =
    do
        args <- getArgs
        
        if (length args /= 1) then
            putStrLn "Usage: cmm [input_file]"
        else do
            handle <- openFile (args!!0) ReadMode
            contents <- hGetContents handle
            
            let tree = parse declaration_list contents
            
            let (List l) = tree
            let tree' = run_passes passes tree
            prettyprint_tree tree'
            let funcs = getFunctions tree'
            let blocks = map reduce $ map blockify funcs
            mapM print blocks
            mapM (print_blocks 0) blocks
            hClose handle
