module DataType where
newtype Type = Type String   deriving (Show,Eq)
data Term = Term {termName :: String, arguments :: [Term]}   deriving (Show,Eq)
newtype Signature = Signature [FunctionSymbol]   deriving (Show,Eq)
data FunctionSymbol = FunctionSymbol {symbol :: String, arguments :: [Type], funcType :: Type}   deriving (Show,Eq)
data Error = ORDER | LENGTH | TYPE | SYMBOL   deriving (Show,Eq)
data Mode = NONE {symbol' :: (Maybe String)} | NO {symbol' :: (Maybe String)} | ONCE {symbol' :: (Maybe String)} deriving (Show,Eq)
