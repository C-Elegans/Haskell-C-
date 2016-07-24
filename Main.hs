module Main where
import Parse
import Eval
import Tree
import Blocks
import SSA
import TempCodegen
import System.Environment
import System.IO
import qualified Data.Map
import Control.Monad.State
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
            let locals = map getLocals funcs
            print locals
            hClose handle
