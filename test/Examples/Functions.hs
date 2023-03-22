module Examples.Functions where

import DataType
import Data.List (find)

allFunctions :: Signature -> [Symbol]
allFunctions (Signature fs) = filter (not . null . #arguments) fs

allConstants :: Signature -> [Symbol]
allConstants (Signature fs) = filter (null . #arguments) fs

theType :: Signature -> String -> Maybe Type
theType (Signature fs) s = fmap #result (find ((== s) . #symbol) fs)

theArgumentsTypes :: Signature -> String -> Maybe [Type]
theArgumentsTypes (Signature fs) s = fmap #arguments (find ((== s) . #symbol) fs)

termSymbols :: Term a -> [a]
termSymbols = toList
