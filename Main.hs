module Main where
import Parse
import Eval
import Tree
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
            --prettyprint_tree tree
            let (List l) = tree
            let tree' = post_apply (\tree -> trace (show tree) tree) tree
            print tree'
            hClose handle
