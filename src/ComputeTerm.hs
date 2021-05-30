module ComputeTerm (
   term,
   termsOfType,
   printTerm,
   printTermsOfType,
   checkLength,
   checkElement,
   makeSingleTerms,
   checkType,
   makeSymbol,
   makeTerm,
   checkExist,
   checkType',
   giveType,
   makeSymbol',
   toTerm',
   makeTerm',
   toString,
   toEnd,
   term'
   )where
import Data.List
import DataType
-- for #3
-- first parameter is a bound on complexity of terms
term :: Int -> Signature -> [Term]
term n (Signature xs) = if maximum(checkLength b) <= n 
                        then term' n (Signature xs) d
                        else []
                        where a = checkElement xs
                              b = xs \\ a 
                              c = makeSingleTerms a ++ makeTerm a b
                              d = makeSingleTerms a ++ makeTerm' xs b c 

-- maybe also for terms of a specific type
termsOfType :: Int -> Type -> Signature -> [Term]
termsOfType n t (Signature xs) = if null a 
                                 then []
                                 else checkType' t (term n (Signature xs)) xs  
                                 where a = term n (Signature xs)

-- can tansform [Term] in more readable forms
printTerm :: Int -> Signature -> [String]
printTerm n xs = nub (map toEnd (term n xs))

printTermsOfType :: Int -> Type -> Signature -> [String]
printTermsOfType n t xs = nub (map toEnd (termsOfType n t xs))

checkLength :: [FunctionSymbol] -> [Int]
checkLength [] = []
checkLength (FunctionSymbol _ xs _:ys) = length xs : checkLength ys


checkElement :: [FunctionSymbol] -> [FunctionSymbol]
checkElement [] = []
checkElement (FunctionSymbol x xs t:ys)
   | null xs = (FunctionSymbol x xs t) : checkElement ys
   | otherwise = [] ++ checkElement ys

makeSingleTerms :: [FunctionSymbol] -> [Term]
makeSingleTerms [] = []
makeSingleTerms (FunctionSymbol x xs t:ys) = Term x [FunctionSymbol x xs t] : makeSingleTerms ys

checkType :: Type -> [FunctionSymbol] -> FunctionSymbol
checkType _ [] = FunctionSymbol "o" [] O
checkType a (FunctionSymbol x xs t:ys) 
   | a == t = FunctionSymbol x xs t
   | otherwise = checkType a ys

makeSymbol :: [FunctionSymbol] -> FunctionSymbol -> [FunctionSymbol] 
makeSymbol _ (FunctionSymbol _ [] _) = []
makeSymbol w (FunctionSymbol a (x:xs) y) = checkType x w : makeSymbol w (FunctionSymbol a xs y)

makeTerm :: [FunctionSymbol] -> [FunctionSymbol] -> [Term]
makeTerm _ [] = []
makeTerm w (FunctionSymbol a x y : xs)
   | checkExist n = Term a n : makeTerm w xs
   | otherwise = makeTerm w xs
   where n = makeSymbol w (FunctionSymbol a x y)

checkExist :: [FunctionSymbol] -> Bool
checkExist xs = not (FunctionSymbol "o" [] O `elem` xs)

checkType' :: Type -> [Term] -> [FunctionSymbol] -> [Term]
checkType' _ [] _ = []
checkType' a (x:xs) ys
   | a == giveType x ys = x : checkType' a xs ys
   | otherwise = checkType' a xs ys

giveType :: Term -> [FunctionSymbol] -> Type
giveType (Term _ _) [] = O
giveType (Term a s) (FunctionSymbol b _ t : ys)
   | a == b = t 
   | otherwise =  giveType (Term a s) ys
giveType (Function _ _) [] = O
giveType (Function a s) (FunctionSymbol b _ t : ys)
   | a == b = t 
   | otherwise =  giveType (Function a s) ys

makeSymbol' :: [FunctionSymbol] -> [Term] -> FunctionSymbol -> [[Term]]
makeSymbol' _ _ (FunctionSymbol _ [] _) = []
makeSymbol' w s (FunctionSymbol a (x:xs) y) = checkType' x s w : makeSymbol' w s (FunctionSymbol a xs y)

toTerm' :: String -> [[Term]] -> [Term]
toTerm' _ [] = []
toTerm' s (x:xs) 
   | null xs = [Function s x] 
   | otherwise = Function s x : toTerm' s xs

makeTerm' :: [FunctionSymbol] -> [FunctionSymbol] -> [Term] -> [Term]
makeTerm' _ [] _ = []
makeTerm' w (FunctionSymbol a x y : xs) t = toTerm' a (sequence(makeSymbol' w t (FunctionSymbol a x y))) ++ makeTerm' w xs t

toString :: [FunctionSymbol] -> [String]
toString [] = []
toString (FunctionSymbol x _ _ :ys) = x : toString ys

toEnd :: Term -> String
toEnd (Term _ []) = "Error"
toEnd (Term x (FunctionSymbol y xs t:ys)) 
   | x == y && null ys = x
   | otherwise = x ++ "(" ++ intercalate "," (toString (FunctionSymbol y xs t:ys)) ++ ")"
toEnd (Function s xs) = s ++ "(" ++ intercalate "," (map toEnd xs) ++ ")"

term' :: Int -> Signature -> [Term] -> [Term]
term' n (Signature xs) w = if null (makeTerm' xs b w \\ w)
                           then makeTerm' xs b w
                           else term' n (Signature xs) (w ++ (makeTerm' xs b w \\ w))
                           where a = checkElement xs
                                 b = xs \\ a 
