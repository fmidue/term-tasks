module DataType where
newtype Type = Type String   deriving (Show,Eq)
data Term = Term {termName :: String, parameter :: [Term]}   deriving (Show,Eq)
newtype Signature = Signature [FunctionSymbol]   deriving (Show,Eq)
data FunctionSymbol = FunctionSymbol {funName :: String, arguments :: [Type], funType :: Type}   deriving (Show,Eq)
data Error = ORDER | LENGTH | TYPE   deriving (Show,Eq)
