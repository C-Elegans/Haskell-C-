module Backends.D16Naive.Backend where
import Parse (Tree(GlobalDec, List))
import Instructions
import Tree (getFunctions, getGlobals )
import TempCodegen (codegen, Location(Global), getLocals, assemble_strings)
import Optimize (optimize)
import qualified Data.Map as M (empty)

runBackend :: Tree -> [(String,String)] -> String -> ([Instruction],String)
runBackend tree strings cleanFileName =
    let globals = getGlobals tree
        globalList = [(str,Global) | (GlobalDec t str) <- globals]

        funcs = getFunctions tree
        locals = map getLocals funcs

        pairs = zip funcs locals
        globalCode = codegen [((List globals),M.empty)] [] cleanFileName
        code = globalCode ++ (codegen pairs globalList cleanFileName)
        assembledStrings = assemble_strings strings cleanFileName
        betterCode = optimize code
    in  (betterCode,assembledStrings)
