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

import DataType (Symbol(..),Signature(..),Type)
import Data.List (nub,find)

allSymbols :: Signature -> [String]
allSymbols (Signature fs) = map symbol fs

allFunctions :: Signature -> [Symbol]
allFunctions (Signature fs) = filter (not . null . arguments) fs

allConstants :: Signature -> [Symbol]
allConstants (Signature fs) = filter (null . arguments) fs

allTypes :: Signature -> [Type]
allTypes (Signature fs) = nub (concatMap arguments fs ++ map #result fs)

theType :: Signature -> String -> Maybe Type
theType (Signature fs) s = fmap #result (find ((== s) . symbol) fs)

theArgumentsTypes :: Signature -> String -> Maybe [Type]
theArgumentsTypes (Signature fs) s = fmap arguments (find ((== s) . symbol) fs)

allSameTypes :: Signature -> Type -> [Symbol]
allSameTypes (Signature fs) t = filter ((== t) . #result) fs



