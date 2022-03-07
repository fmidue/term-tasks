module Auxiliary (
  different,
) where

import Test.QuickCheck (Gen, elements, suchThat)

different :: Eq a => [a] -> Int -> Gen [a]
different _ 0 = return []
different es n = do
    next <- different es (n-1)
    e <- elements es `suchThat` (`notElem` next)
    return (e:next)
