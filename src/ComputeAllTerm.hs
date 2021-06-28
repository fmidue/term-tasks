module ComputeAllTerm where
import DataType
import ComputeTerm
import ArbitraryTerm
import Test.QuickCheck

allTerm1 :: Int -> Signature -> Gen [Term]
allTerm1 n xs = do
    invalidT <- invalidTerm1 n xs
    let validT = term n xs
    combineT <- shuffle (invalidT++validT)
    return (take n combineT)

allTerm2 :: Int -> Signature -> Gen [Term]
allTerm2 n xs = do
    invalidT <- invalidTerm2 n xs
    let validT = term n xs
    combineT <- shuffle (invalidT++validT)
    return (take n combineT)

allTermOfType1 :: Int -> Type -> Signature -> Gen [Term]
allTermOfType1 n t xs = do
    invalidT <- invalidTermOfType1 n t xs
    let validT = termsOfType n t xs
    combineT <- shuffle (invalidT++validT)
    return (take n combineT)

allTermOfType2 :: Int -> Type -> Signature -> Gen [Term]
allTermOfType2 n t xs = do
    invalidT <- invalidTermOfType2 n t xs
    let validT = termsOfType n t xs
    combineT <- shuffle (invalidT++validT)
    return (take n combineT)

--checkAllTerm :: Int -> Signature -> Gen [Bool]
--checkAllTerm n xs = do
--    a <- allTerm n xs
--    let b = map (isValid xs) a
--    return b

--main :: IO ()
--main = do
--    let a = 10
--        b = Signature [FunctionSymbol "x" [] (Type "A"),FunctionSymbol "y" [] (Type "B"),FunctionSymbol "z" [] (Type "C"),FunctionSymbol "f" [Type "A",Type "A"] (Type "B"),FunctionSymbol "g" [Type "A",Type "B"] (Type "C"),FunctionSymbol "h" [Type "A",Type "B",Type "C"] (Type "D")]
--    c <- generate (allTerm a b)
--    let d = map (isValid b) c
--    print c
--    print d
