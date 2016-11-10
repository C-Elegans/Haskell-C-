module Backends.D16Hoopl.ToIR where
import Backends.D16Hoopl.IR
import Compiler.Hoopl
import Parse (Tree)

treeToIR :: Tree -> Graph (Block Node) O O
treeToIR tree = GNil


