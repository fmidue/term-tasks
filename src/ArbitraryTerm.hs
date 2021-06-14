module ArbitraryTerm where
import Data.List
import Test.QuickCheck
import DataType
import ComputeTerm 

changeOrder :: Term -> Gen Term
changeOrder (Term x) = return (Term x)
changeOrder (Function x xs) = do
  a <- shuffle xs 
  return (Function x a)                 

getInt :: Maybe Int -> Int
getInt (Just x) = x
getInt Nothing = -1

getList :: Term -> [Term]
getList (Term _) = []
getList (Function _ xs) = xs

getName :: Term -> String
getName (Term _) = "Error"
getName (Function x _) = x

changeLength :: Term -> Gen Term
changeLength (Term x) = return (Term x)
changeLength (Function x xs) = do
  a <- choose (0,(length xs)+5)
  let b = elements xs
  c <- vectorOf a b
  return (Function x c)

changeName :: Term -> Gen Term 
changeName (Term _) = do 
  let a = ['a'..'z']
      b = map makeString a  
  c <- elements b
  let d = Term c 
  return d
changeName (Function x xs) = do  
  let a = ['a'..'z']
      b = map makeString a  
  c <- elements b
  d <- choose (0,length xs)
  if (d == length xs)
  then return (Function c xs)
  else do let e = changeName' (xs !! d) c 
              f = replace d e xs
          return (Function x f)

changeName' :: Term -> String -> Term
changeName' (Term _) x = Term x
changeName' (Function _ xs) x = Function x xs

replace :: Int -> Term -> [Term] -> [Term]
replace _ _ [] = []
replace n y (x:xs) 
  | n == 0 = y : replace (n-1) y xs
  | otherwise = x : replace (n-1) y xs

makeString :: Char -> String
makeString x = [x]

changeFunction :: Term -> Gen Term
changeFunction (Term x) = return (Term x)
changeFunction (Function x xs) = do
  a <- elements xs
  if checkTerm a
  then changeFunction (Function x xs)
  else do b <- elements [1 :: Int,2,3]
          let d = elemIndex a xs 
          if b == 1
          then do c <- changeOrder a 
                  let f = replace (getInt d) c xs
                  return (Function x f)
          else if b == 2
          then do c <- changeLength a 
                  let f = replace (getInt d) c xs
                  return (Function x f)
          else do c <- changeName a 
                  let f = replace (getInt d) c xs
                  return (Function x f)

checkTerm :: Term -> Bool
checkTerm (Term _) = True
checkTerm (Function _ _) = False          

randomTerm :: Term -> Gen Term
randomTerm (Term x) = do 
  a <- elements [1 :: Int,2]
  if a == 1
  then return (Term x)
  else changeName (Term x)
randomTerm (Function x xs) = do  
  a <- elements [1 :: Int,4]
  let b = ["changeOrder","changeLength","changeName","changeFunction"]
      c = elements b
  d <- vectorOf a c
  randomTerm' (Function x xs) d

randomTerm' :: Term -> [String] -> Gen Term
randomTerm' _ [] = return (Term "Error") 
randomTerm' t ["changeOrder"] = changeOrder t
randomTerm' t ["changeLength"] = changeLength t
randomTerm' t ["changeName"] = changeName t
randomTerm' t ["changeFunction"] = changeFunction t
randomTerm' t (x:xs) 
  | x == "changeOrder" = do a <- changeOrder t 
                            randomTerm' a xs
  | x == "changeLength" = do a <- changeLength t 
                             randomTerm' a xs                         
  | x == "changeName" = do a <- changeName t 
                           randomTerm' a xs
  | x == "changeFunction" = do a <- changeFunction t 
                               randomTerm' a xs
  | otherwise = return (Term "Error")

printRandomTerm :: Term -> Gen String
printRandomTerm t = do
  a <- randomTerm t
  return (toEnd a)


