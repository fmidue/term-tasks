module DataType where
data Type = Type String   deriving (Show,Eq)
data Term = Term String [Term]   deriving (Show,Eq)
newtype Signature = Signature [FunctionSymbol]   deriving (Show,Eq)
data FunctionSymbol = FunctionSymbol String [Type] Type   deriving (Show,Eq)