module Main where
import Parse
import Eval
import Tree
import Blocks
import SSA
import Optimize (optimize)
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
            
            tree <- parse declaration_list contents
            prettyprint_tree tree
            let (List l) = tree
            let (tree',strings) = run_tree tree
            prettyprint_tree tree'
            let globals = getGlobals tree'
            let globalList = [(str,Global) | (GlobalDec t str) <- globals]
            
            let funcs = getFunctions tree'
            let locals = map getLocals funcs
            
            let pairs = zip funcs locals
            print strings
            let code = codegen pairs globalList cleanFileName
            mapM_ print code
            putStrLn "\n"
            let assembledStrings = assemble_strings strings cleanFileName
            let betterCode = optimize code
            mapM_ print betterCode
            let outFileName = (args!!1)
            let codeString = concat $ intersperse "\n" $ map show betterCode
            print codeString
            putStrLn $ "Reduced code length by: " ++ (show (round $ 100 * (1- ((fromIntegral $ length betterCode) / (fromIntegral $ length code))))) ++ "%"
            out_handle <- openFile outFileName WriteMode
            hPutStr out_handle (codeString ++ "\nend:\n\n" ++ assembledStrings)
            
            hClose handle
            hClose out_handle
