{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataType where

import GHC.OverloadedLabels
import GHC.Records

newtype Type = Type String   deriving (Show,Eq)
data Term = Term {symbol :: String, arguments :: [Term]}   deriving (Show,Eq)
newtype Signature = Signature [Symbol]   deriving (Show,Eq)
data Symbol = Symbol {symbol :: String, arguments :: [Type], result :: Type}   deriving (Show,Eq)
data Error = SWAP | DUPLICATE | TYPE | ONEMORE | ONELESS   deriving (Show,Eq)

-- IsLabel orphan instance for (->) --
instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x
