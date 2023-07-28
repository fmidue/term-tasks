{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language DeriveGeneric #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}

module DataType where

import GHC.Generics
import GHC.OverloadedLabels
import GHC.Records
import Data.List (nub,intercalate)
import Test.QuickCheck (Arbitrary(..), elements)


newtype Type = Type {name :: String}   deriving (Eq,Generic)

data Term a = Term {symbol :: a, arguments :: [Term a]}
  deriving (Eq, Generic, Functor, Foldable)

newtype Signature = Signature { definitions :: [Symbol]}  deriving (Generic)
data Symbol = Symbol {symbol :: String, arguments :: [Type], result :: Type} deriving Generic

data Error = Swap | TypeChange | OneMore | OneLess | NameTypo | UnknownSymbol
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic)

instance Arbitrary Error where
  arbitrary = elements [minBound .. maxBound]

instance Show Symbol where
  show (Symbol s ts (Type t)) = s ++ " : " ++ if null ts then t else intercalate " x " (map (\(Type s') -> s') ts) ++ " -> " ++ t

instance Show (Term String) where
  show = transTerm
    where
      transTerm :: Term String -> String
      transTerm (Term x []) = x
      transTerm (Term s xs) = s ++ "(" ++ intercalate "," (map transTerm xs) ++ ")"

{-|
The total amount of occurrences of symbols within the given 'Term'.
-}
termSize :: Term a -> Int
termSize = length

instance Show Signature where
  show = show . definitions


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
