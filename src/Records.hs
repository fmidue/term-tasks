{-# LANGUAGE DeriveGeneric #-}

module Records where

import Data.Typeable
import GHC.Generics

import DataType (Error(..), Signature, Symbol(..), Term(..), Type(..), toSignature)



data Base = Base
            { termSizeRange :: (Int,Int)
            , wrongTerms :: [(Int,Error)]
            , properTerms :: Int
            , extraFeedback :: Bool
            , printSolution :: Bool
            } deriving (Typeable, Generic)

dBase :: Base
dBase = Base
        { termSizeRange = (6,10)
        , wrongTerms = [ (1,Swap), (1,TypeChange), (1,OneMore), (1,OneLess) ]
        , properTerms = 5
        , extraFeedback = False
        , printSolution = False
        }




data Certain = Certain {
                 signatures :: Signature
               , root :: Maybe [Type]
               , baseConf :: Base
               } deriving (Typeable, Generic)

dCertain :: Certain
dCertain = Certain
           { signatures = toSignature [("d",[],"R"),("e",[],"S"),("f",[],"T"),("g",["R","R"],"S"),("h",["R","S"],"T"),("i",["R","S","T"],"U")]
           , root = Nothing
           , baseConf = dBase
           }



data Perturbed = Perturbed {
                 symbols :: [String]
               , types :: [String]
               , sigs :: [([Int],Int)]
               , root :: Maybe [Int]
               , baseConf :: Base
               } deriving (Typeable, Generic)

dPerturbed :: Perturbed
dPerturbed = Perturbed
           { symbols = ["d","e","f","g","h","i"]
           , types = ["R","S","T","U"]
           , sigs = [ ([],1), ([],2), ([],3), ([1,1],2), ([1,2],3), ([1,2,3],4) ]
           , root = Nothing
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
                    , terms :: [Term String]
                    , correct :: [Int]
                    , moreFeedback :: Bool
                    , showSolution :: Bool
                    } deriving (Typeable, Generic)

dSigInst :: SigInstance
dSigInst = SigInstance {
              symbols = [Symbol "f" [Type "a", Type "b"] (Type "c"), Symbol "g" [] (Type "a"), Symbol "h" [Type "a"] (Type "b")]
            , terms = [Term "f" [Term "g" [], Term "h" [Term "g" []]], Term "g" [Term "h" []], Term "f" [Term "h" []]]
            , correct = [1]
            , moreFeedback = False
            , showSolution = False
            }
