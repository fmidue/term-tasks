{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DataType where

import GHC.OverloadedLabels

newtype Type = Type String   deriving (Show,Eq)
data Term = Term {termName :: String, arguments :: [Term]}   deriving (Show,Eq)
newtype Signature = Signature [FunctionSymbol]   deriving (Show,Eq)
data FunctionSymbol = FunctionSymbol {symbol :: String, arguments :: [Type], funcType :: Type}   deriving (Show,Eq)
data Error = ORDER | LENGTH | TYPE | SYMBOL   deriving (Show,Eq)

-- IsLabel instances --

-- Term
instance IsLabel "termName" (Term -> String) where
  fromLabel = termName
instance IsLabel "arguments" (Term -> [Term]) where
  fromLabel = arguments

-- FunctionSymbol
instance IsLabel "symbol" (FunctionSymbol -> String) where
  fromLabel = symbol
instance IsLabel "arguments" (FunctionSymbol -> [Type]) where
  fromLabel = arguments
instance IsLabel "funcType" (FunctionSymbol -> Type) where
  fromLabel = funcType
