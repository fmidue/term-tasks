module CheckError (
   checkErrorType
   ) where

import DataType
import Data.Maybe (fromJust)
import Data.List ((\\))
import ValidCheck (isValid)
import GetSignatureInfo (giveArgType,giveType)
import DealWithTerm (getArgSymbol)


checkErrorType :: Signature -> Term -> Maybe Error
checkErrorType xs t
   | isValid xs t = Nothing
   | not (lengthError xs t) = Just LENGTH
   | not (typeError xs t) = Just TYPE
   | orderError xs t = Just ORDER
   | otherwise = Nothing

--errorTypeList :: Signature -> Term -> [Error]

lengthError :: Signature -> Term -> Bool
lengthError xs (Term s ys) = length ys == length (fromJust(giveArgType xs s)) && lengthError' xs ys

lengthError' :: Signature -> [Term] -> Bool
lengthError' _ [] = True
lengthError' xs (y:ys) = lengthError xs y && lengthError' xs ys

orderError :: Signature -> Term -> Bool
orderError xs (Term s ys) = compareType tList (fromJust (giveArgType xs s)) && orderError' xs ys
                              where tList = map (fromJust . giveType xs) (getArgSymbol ys)

orderError' :: Signature -> [Term] -> Bool
orderError' _ [] = True
orderError' xs (y:ys) = orderError xs y && orderError' xs ys

compareType :: [Type] -> [Type] -> Bool
compareType [] [] = True
compareType [] _ = False
compareType (t:ts) xs
   | t `elem` xs = compareType ts (xs\\[t])
   | otherwise = compareType ts xs

typeError :: Signature -> Term -> Bool
typeError xs (Term s ys) = all (`elem` sigType) argType && all (`elem` argType) sigType && typeError' xs ys
                              where sigType = fromJust (giveArgType xs s)
                                    argType = map (fromJust . giveType xs) (getArgSymbol ys)

typeError' :: Signature -> [Term] -> Bool
typeError' _ [] = True
typeError' xs (y:ys) = typeError xs y && typeError' xs ys
