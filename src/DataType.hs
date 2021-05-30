module DataType where
data FunctionSymbol = FunctionSymbol String [Type] Type   deriving (Show,Eq)
data Type = A | B | C | D | E | O   deriving (Show,Eq)
data Term = Term String [FunctionSymbol] | Function String [Term]   deriving (Show,Eq)   
newtype Signature = Signature [FunctionSymbol]   deriving (Show,Eq)    