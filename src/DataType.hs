{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataType where

import GHC.OverloadedLabels
import GHC.Records
import Data.List (nub,intercalate)

newtype Type = Type String   deriving Eq
data Term = Term {symbol :: String, arguments :: [Term]}   deriving Eq
newtype Signature = Signature [Symbol]
data Symbol = Symbol {symbol :: String, arguments :: [Type], result :: Type}
data Error = SWAP | TYPE | ONEMORE | ONELESS | SYMBOL | SYMBOLTYPE   deriving (Show,Read,Bounded,Enum)

instance Show Signature where
  show sig = "[ " ++ intercalate ", " (transSignature sig) ++ " ]"
    where
      transSignature :: Signature -> [String]
      transSignature (Signature fs) = map (\(Symbol s ts (Type t))-> s ++ ":" ++ concatMap (\(Type s')->s' ++ "->") ts ++ t) fs

instance Show Term where
  show = transTerm
    where
      transTerm :: Term -> String
      transTerm (Term x []) = x
      transTerm (Term s xs) = s ++ "(" ++ intercalate "," (map transTerm xs) ++ ")"

-- IsLabel orphan instance for (->) --
instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

allSymbols :: Signature -> [String]
allSymbols (Signature fs) = map #symbol fs

allTypes :: Signature -> [Type]
allTypes (Signature fs) = nub (concatMap #arguments fs ++ map #result fs)

allArgsResults :: Signature -> [([Type],Type)]
allArgsResults (Signature fs) = map (\x-> (#arguments x,#result x)) fs

allSameTypes :: Signature -> Type -> [Symbol]
allSameTypes (Signature fs) t = filter ((== t) . #result) fs

toSignature :: [(String,[String],String)] -> Signature
toSignature list = Signature (map (\(s,ts,r)->Symbol s (map Type ts) (Type r)) list)
