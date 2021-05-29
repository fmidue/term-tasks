module DataType where
data FunctionSymbol = FunctionSymbol String [Type] Type   deriving (Show,Eq)
data Type = A | B | C | D | E   deriving (Show,Eq)
data Term = Term1 String [FunctionSymbol] | Term2 String [FunctionSymbol] Type   deriving (Show,Eq)   
data Signature = Signature [FunctionSymbol]   deriving (Show,Eq)    