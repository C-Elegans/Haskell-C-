module Main where
import Parse
import Eval
import Tree
import Blocks
import SSA
import TempCodegen
import System.Environment
import System.IO
import Data.List (intersperse)
import System.FilePath
import qualified Data.Map
import Control.Monad.State
import Debug.Trace (trace)
main =
    do
        args <- getArgs
        
        if (length args /= 2) then
            putStrLn "Usage: cmm [input_file] [output_file]"
        else do
            let filename = (args!!0)
            let cleanFileName = fst $ splitExtension filename
            handle <- openFile filename ReadMode
            contents <- hGetContents handle
            
            let tree = parse declaration_list contents
            
            let (List l) = tree
            let tree' = run_passes passes tree
            prettyprint_tree tree'
            let funcs = getFunctions tree'
            let locals = map getLocals funcs
            let pairs = zip funcs locals
            print pairs
            print locals
            let code = codegen pairs cleanFileName
            mapM_ print code
            let outFileName = (args!!1)
            let codeString = concat $ intersperse "\n" $ map show code
            print codeString
            out_handle <- openFile outFileName WriteMode
            hPutStr out_handle (codeString ++ "\nend:\n\n")
            
            hClose handle
            hClose out_handle
