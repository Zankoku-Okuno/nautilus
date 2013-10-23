module Nautilus.Frontend.Analyze where
import Data.HashMap (HashMap, lookup, insert)
import Nautilus.Frontend.Types

data Declaration = TypeDecl  Identifier Visibility Type
                 | ValDecl   Identifier Integer
                 | VarDecl   Identifier Type
                 | FunDecl   [(Identifier, Type)]
                 | LabelDecl Identifier
type DeclMap = HashMap String Declaration

data Scope = Local Scope DeclMap
           | Global DeclMap [Scope]
