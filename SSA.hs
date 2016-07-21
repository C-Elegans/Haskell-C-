module SSA where
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

type SSATab = M.Map String (Int)
type SSAState a = State SSATab a

data SSAVar = SSAVar String Int

instance Show SSAVar where
    show (SSAVar str x) = str ++ "_" ++ (show x)

newSVar :: String -> SSAState (SSAVar)
newSVar str = do
    tab <- get
    let val = M.lookup str tab
    case val of
        (Just x) -> do
            put $ M.insert str (x+1) tab    
            return (SSAVar str x)
        (Nothing) -> do
            put $ M.insert str 1 tab
            return (SSAVar str 0)

