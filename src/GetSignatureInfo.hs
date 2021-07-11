module GetSignatureInfo (
   getSigSymbol,
   getAllConstant,
   getAllFunction,
   getAllType,
   giveType,
   giveArgType,
   getAllSameType
)
where

import DataType
import Data.List (nub,find)

getSigSymbol :: Signature -> [String]
getSigSymbol (Signature fs) = map funcName fs

getAllFunction :: Signature -> [FunctionSymbol]
getAllFunction (Signature fs) = filter (not . null . arguments) fs

getAllConstant :: Signature -> [FunctionSymbol]
getAllConstant (Signature fs) = filter (null . arguments) fs

getAllType :: Signature -> [Type]
getAllType (Signature fs) = nub (concatMap arguments fs ++ map funcType fs)

giveType :: Signature -> String -> Maybe Type
giveType (Signature fs) s = fmap funcType (find ((== s) . funcName) fs)

giveArgType :: Signature -> String -> Maybe [Type]
giveArgType (Signature fs) s = fmap arguments (find ((== s) . funcName) fs)

getAllSameType :: Signature -> Type -> [FunctionSymbol]
getAllSameType (Signature fs) t = filter ((== t) . funcType) fs



