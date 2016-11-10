module Main where
import Parse
import Type
import Tree
import System.Environment
import System.IO
import System.Exit
import Data.List (intersperse)
import System.FilePath
import qualified Data.Map
import Control.Monad.State
import Debug.Trace (trace)
--import Backends.D16Naive.Backend (runBackend)
import Backends.D16Hoopl.Backend (runBackend)

import qualified Data.Map as M (empty)
main =
    do
        args <- getArgs
        
        if (length args /= 2) then do
            putStrLn "Usage: cmm [input_file] [output_file]"
            exitFailure
        else do
            let filename = (args!!0)
            let cleanFileName = fst $ splitExtension filename
            handle <- openFile filename ReadMode
            contents <- hGetContents handle
            
            let parsed_tree = parse declaration_list cleanFileName contents
            case parsed_tree of
                Left err -> do
                    putStrLn (show err)
                    exitFailure
                Right tree ->do
                    --prettyprint_tree tree
                    let (List l) = tree
                    let (tree',strings) = run_tree tree
                    prettyprint_tree tree'
                    print tree'
                    let (code,assembledStrings) = runBackend tree' strings cleanFileName
                    
                   
                    mapM_ print code
                    let outFileName = (args!!1)
                    let codeString = concat $ intersperse "\n" $ map show code
            
                    
                    out_handle <- openFile outFileName WriteMode
                    hPutStr out_handle (codeString ++ "\nend:\n\n" ++ assembledStrings)
            
                    hClose handle
                    hClose out_handle
