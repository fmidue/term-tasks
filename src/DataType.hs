{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataType where

import GHC.OverloadedLabels
import GHC.Records
import Data.List (nub,find,intercalate)

newtype Type = Type String   deriving (Show,Eq)
data Term = Term {symbol :: String, arguments :: [Term]}   deriving (Show,Eq)
newtype Signature = Signature [Symbol]   deriving (Show,Eq)
data Symbol = Symbol {symbol :: String, arguments :: [Type], result :: Type}   deriving (Show,Eq)
data Error = SWAP | TYPE | ONEMORE | ONELESS | SYMBOL | SYMBOLTYPE   deriving (Show,Eq,Read)

-- IsLabel orphan instance for (->) --
instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

allSymbols :: Signature -> [String]
allSymbols (Signature fs) = map #symbol fs

allFunctions :: Signature -> [Symbol]
allFunctions (Signature fs) = filter (not . null . #arguments) fs

allConstants :: Signature -> [Symbol]
allConstants (Signature fs) = filter (null . #arguments) fs

allTypes :: Signature -> [Type]
allTypes (Signature fs) = nub (concatMap #arguments fs ++ map #result fs)

allArguments :: Signature -> [[Type]]
allArguments (Signature fs) = map #arguments fs

theType :: Signature -> String -> Maybe Type
theType (Signature fs) s = fmap #result (find ((== s) . #symbol) fs)

theArgumentsTypes :: Signature -> String -> Maybe [Type]
theArgumentsTypes (Signature fs) s = fmap #arguments (find ((== s) . #symbol) fs)

allSameTypes :: Signature -> Type -> [Symbol]
allSameTypes (Signature fs) t = filter ((== t) . #result) fs

termSymbols :: Term -> [String]
termSymbols (Term x xs) = x : termSymbols' xs

termSymbols' :: [Term] -> [String]
termSymbols' = concatMap termSymbols

transTerm :: Term -> String
transTerm (Term x []) = x
transTerm (Term s xs) = s ++ "(" ++ intercalate "," (map transTerm xs) ++ ")"

toType :: [String] -> [Type]
toType = map Type