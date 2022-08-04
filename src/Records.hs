{-# LANGUAGE TupleSections #-}

module Records where

import DataType (Error(..), Signature, toSignature)



data Base = Base
            { termSizeRange :: (Int,Int)
            , wrongTerms :: [(Int,Error)]
            , properTerms :: Int
            }

dBase :: Base
dBase = Base
        { termSizeRange = (6,10)
        , wrongTerms = [(1,SWAP),(1,TYPE),(1,ONEMORE),(1,ONELESS)]
        , properTerms = 5
        }




data Certain = Certain {
                 signatures :: Signature
               , baseConf :: Base
               }

dCertain :: Certain
dCertain = Certain
           { signatures = toSignature [("d",[],"R"),("e",[],"S"),("f",[],"T"),("g",["R","R"],"S"),("h",["R","S"],"T"),("i",["R","S","T"],"U")]
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