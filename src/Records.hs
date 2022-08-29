{-# LANGUAGE TupleSections, DeriveGeneric #-}

module Records where

import Data.Typeable
import GHC.Generics

import DataType (Error(..), Signature, Symbol, Term, toSignature)



data Base = Base
            { termSizeRange :: (Int,Int)
            , wrongTerms :: [(Int,Error)]
            , properTerms :: Int
            } deriving (Typeable, Generic)

dBase :: Base
dBase = Base
        { termSizeRange = (6,10)
        , wrongTerms = [(1,SWAP),(1,TYPE),(1,ONEMORE),(1,ONELESS)]
        , properTerms = 5
        }




data Certain = Certain {
                 signatures :: Signature
               , baseConf :: Base
               } deriving (Typeable, Generic)

dCertain :: Certain
dCertain = Certain
           { signatures = toSignature [("d",[],"R"),("e",[],"S"),("f",[],"T"),("g",["R","R"],"S"),("h",["R","S"],"T"),("i",["R","S","T"],"U")]
           , baseConf = dBase
           }



data Perturbed = Perturbed {
                 symbols :: [String]
               , types :: [String]
               , sigs :: [([Int],Int)]
               , baseConf :: Base
               } deriving (Typeable, Generic)

dPerturbed :: Perturbed
dPerturbed = Perturbed
           { symbols = ["d","e","f","g","h","i"]
           , types = ["R","S","T","U"]
           , sigs = [ ([],1), ([],2), ([],3), ([1,1],2), ([1,2],3), ([1,2,3],4) ]
           , baseConf = dBase
           }



data Random = Random {
                 symbols :: [String]
               , types :: [String]
               , maxArgs :: Int
               , baseConf :: Base
               }

dRandom :: Random
dRandom = Random
           { symbols = ["f","g","h"]
           , types = ["A","B","C"]
           , maxArgs = 5
           , baseConf = dBase
           }



data SigInstance = SigInstance {
                       symbols :: [Symbol]
                    ,  terms :: [Term]
                    ,  correct :: [Int]
                    } deriving (Typeable, Generic)
