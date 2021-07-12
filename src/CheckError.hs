module CheckError (
   errorType
   ) where

import DataType
import Data.Maybe (fromJust)
import Data.List ((\\))
import ValidCheck (isValid)
import GetSignatureInfo (theArgumentsTypes,theType)


errorType :: Signature -> Term -> Maybe Error
errorType xs t
   | isValid xs t = Nothing
   | not (lengthError xs t) = Just LENGTH
   | not (typeError xs t) = Just TYPE
   | orderError xs t = Just ORDER
   | otherwise = Nothing

--errorTypeList :: Signature -> Term -> [Error]

lengthError :: Signature -> Term -> Bool
lengthError xs (Term s ys) = length ys == length (fromJust(theArgumentsTypes xs s)) && lengthError' xs ys

lengthError' :: Signature -> [Term] -> Bool
lengthError' _ [] = True
lengthError' xs (y:ys) = lengthError xs y && lengthError' xs ys

orderError :: Signature -> Term -> Bool
orderError xs (Term s ys) = isSameType tList (fromJust (theArgumentsTypes xs s)) && orderError' xs ys
                              where tList = map ((fromJust . theType xs) . termName) ys

orderError' :: Signature -> [Term] -> Bool
orderError' _ [] = True
orderError' xs (y:ys) = orderError xs y && orderError' xs ys

isSameType :: [Type] -> [Type] -> Bool
isSameType [] [] = True
isSameType [] _ = False
isSameType (t:ts) xs
   | t `elem` xs = isSameType ts (xs\\[t])
   | otherwise = isSameType ts xs

typeError :: Signature -> Term -> Bool
typeError xs (Term s ys) = all (`elem` sigType) argType && all (`elem` argType) sigType && typeError' xs ys
                              where sigType = fromJust (theArgumentsTypes xs s)
                                    argType = map ((fromJust . theType xs) . termName) ys

typeError' :: Signature -> [Term] -> Bool
typeError' _ [] = True
typeError' xs (y:ys) = typeError xs y && typeError' xs ys

