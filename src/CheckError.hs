module CheckError (
   checkErrorType
   ) where

import DataType
import Data.Maybe (fromJust)
import Data.List ((\\))
import ValidCheck (isValid)
import GetSignatureInfo (giveArgType,giveTypeList)
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
lengthError xs (Term s ys) = length ys == length (fromJust(giveArgType s xs)) && lengthError' xs ys

lengthError' :: Signature -> [Term] -> Bool
lengthError' _ [] = True
lengthError' xs (y:ys) = lengthError xs y && lengthError' xs ys

orderError :: Signature -> Term -> Bool
orderError xs (Term s ys) = compareType (giveTypeList (getArgSymbol ys) xs) (fromJust (giveArgType s xs)) && orderError' xs ys

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
                              where sigType = fromJust (giveArgType s xs)
                                    argType = giveTypeList (getArgSymbol ys) xs

typeError' :: Signature -> [Term] -> Bool
typeError' _ [] = True
typeError' xs (y:ys) = typeError xs y && typeError' xs ys

