module ComputeAllTerm where
--import DataType
--import ComputeTerm (term,getSameTypeTerm)
--import ValidCheck (isValid)
--import ArbitraryTerm
--import Data.List (partition)
--import Test.QuickCheck

--allTerm :: Int -> Signature -> Gen [Term]
--allTerm n xs = do
--    invalidT <- invalidTerm n xs
--    let validT = term n xs
--    combineT <- shuffle (invalidT++validT)
--    return (take n combineT)

--allTerm2 :: Int -> Signature -> Gen [Term]
--allTerm2 n xs = do
--    invalidT <- invalidTerm2 n xs
--    let validT = term n xs
--    combineT <- shuffle (invalidT++validT)
--    return (take n combineT)

--allTermOfType :: Int -> Type -> Signature -> Gen [Term]
--allTermOfType n t xs = do
--    invalidT <- invalidTermOfType n t xs
--    let validT = getSameTypeTerm t (term n xs) xs
--    combineT <- shuffle (invalidT++validT)
--    return (take n combineT)

--allTermOfType2 :: Int -> Type -> Signature -> Gen [Term]
--allTermOfType2 n t xs = do
--    invalidT <- invalidTermOfType2 n t xs
--    let validT = getSameTypeTerm t (term n xs) xs
--    combineT <- shuffle (invalidT++validT)
--    return (take n combineT)

-- partition :: (a -> Bool) -> [a] -> ([a], [a])
--checkAllTerm :: Int -> Signature -> Gen ([Term],[Term])
--checkAllTerm n xs = do
--    t <- allTerm n xs
--    let termTuple = partition (isValid xs) t
--    return termTuple



--main :: IO ()
--main = do
--    let a = 10
--        b = Signature [FunctionSymbol "x" [] (Type "A"),FunctionSymbol "y" [] (Type "B"),FunctionSymbol "z" [] (Type "C"),FunctionSymbol "f" [Type "A",Type "A"] (Type "B"),FunctionSymbol "g" [Type "A",Type "B"] (Type "C"),FunctionSymbol "h" [Type "A",Type "B",Type "C"] (Type "D")]
--    c <- generate (allTerm a b)
--    let d = map (isValid b) c
--    print c
--    print d
