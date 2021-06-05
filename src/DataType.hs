module DataType where
data FunctionSymbol = FunctionSymbol String [Type] Type   deriving (Show,Eq)
data Type = Type String   deriving (Show,Eq)
data Term = Term String | Function String [Term]   deriving (Show,Eq)   
newtype Signature = Signature [FunctionSymbol]   deriving (Show,Eq)    