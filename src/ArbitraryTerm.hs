module ArbitraryTerm where
import Data.List
import Test.QuickCheck
import DataType
import ComputeTerm
import ValidCheck

changeOrder :: Term -> Gen Term
changeOrder (Term x) = do return (Term x)
changeOrder (Function x xs) = do
  a <- elements [0,1]
  if a == 0
  then do b <- shuffle xs
          return (Function x b) 
  else do b <- elements xs
          if checkTerm b 
          then return (Function x xs)
          else do let c = getList b
                      d = elemIndex b xs
                  e <- shuffle c 
                  let f = getName b
                      g = replace (getInt d) (Function f e) xs
                  return (Function x g)                  

getInt :: Maybe Int -> Int
getInt (Just x) = x
getInt Nothing = -1

getList :: Term -> [Term]
getList (Term _) = []
getList (Function _ xs) = xs

getName :: Term -> String
getName (Term _) = "Error"
getName (Function x _) = x

--randomTerm :: Signature -> Gen Term
--randomTerm (Signature xs) = do  
--  a <- choose (1,20)
--  b <- choose (0,5)
--  let c = elements (term a (Signature xs))
--  d <- vectorOf b c
--  e <- elements (getSigName xs)
--  let f = backTerm e d   
--  return f

changeLength :: Term -> Gen Term
changeLength (Term x) = do return (Term x)
changeLength (Function x xs) = do
  a <- elements [0,1]
  if a == 0 
  then do b <- choose (0,(length xs)+5)
          let c = elements xs
          d <- vectorOf b c
          return (Function x d)
  else do b <- elements xs
          if checkTerm b
          then return (Function x xs)
          else do let c = getList b 
                      d = elemIndex b xs 
                      e = elements c 
                      f = getName b
                  g <- choose (0,(length c)+5)
                  h <- vectorOf g e 
                  let i = replace (getInt d) (Function f h) xs
                  return (Function x i)

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
changeFunction (Term x) = do return (Term x)
changeFunction (Function x xs) = do
  a <- elements xs
  if checkTerm a
  then changeFunction (Function x xs)
  else do b <- elements [1,2,3]
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


