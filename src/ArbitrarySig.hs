module ArbitrarySig (
    arbSignature
)where
import DataType (Signature(..),Symbol(..),Type(..))
import Test.QuickCheck

arbSignature :: [String] -> [Type] -> Int -> Gen Signature
arbSignature s ts n = do
    symbols <- arbSymbols s ts n
    return (Signature symbols)

arbSymbols :: [String] -> [Type] -> Int -> Gen [Symbol]
arbSymbols [] _ _ = return []
arbSymbols (s:ls) ts n = do
    l <- elements [0..n]
    typeList <- vectorOf l (elements ts)
    t <- elements ts
    nextSymbol <- arbSymbols ls ts n
    let thisSymbol = Symbol s typeList t
    return (thisSymbol:nextSymbol)





