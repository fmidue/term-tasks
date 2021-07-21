module GetSignatureInfo (
   allSymbols,
   allConstants,
   allFunctions,
   allTypes,
   theType,
   theArgumentsTypes,
   allSameTypes
)
where

import DataType (FunctionSymbol(..),Signature(..),Type)
import Data.List (nub,find)

allSymbols :: Signature -> [String]
allSymbols (Signature fs) = map symbol fs

allFunctions :: Signature -> [FunctionSymbol]
allFunctions (Signature fs) = filter (not . null . arguments) fs

allConstants :: Signature -> [FunctionSymbol]
allConstants (Signature fs) = filter (null . arguments) fs

allTypes :: Signature -> [Type]
allTypes (Signature fs) = nub (concatMap arguments fs ++ map funcType fs)

theType :: Signature -> String -> Maybe Type
theType (Signature fs) s = fmap funcType (find ((== s) . symbol) fs)

theArgumentsTypes :: Signature -> String -> Maybe [Type]
theArgumentsTypes (Signature fs) s = fmap arguments (find ((== s) . symbol) fs)

allSameTypes :: Signature -> Type -> [FunctionSymbol]
allSameTypes (Signature fs) t = filter ((== t) . funcType) fs



