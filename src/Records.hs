
module Records where

import DataType (Error(..), Signature, toSignature)



data Base = Base
            { termSizeRange :: (Int,Int)
            , wrongTerms :: [(Int,Error)]
            , properTerms :: Int
            }

dBase :: Base
dBase = Base
        { termSizeRange = (1,10)
        , wrongTerms = [(5,SWAP)]
        , properTerms = 5
        }




data Certain = Certain {
                 signatures :: Signature
               , baseConf :: Base
               }

dCertain :: Certain
dCertain = Certain
           { signatures = toSignature [("x",[],"A"),("y",[],"B"),("z",[],"C"),("f",["A","A"],"B"),("g",["A","B"],"C"),("h",["A","B","C"],"D")]
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