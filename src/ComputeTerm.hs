module ComputeTerm (
   term,
   termsOfType,
   printTerm,
   printTermsOfType,
   checkElement,
   makeSingleTerms,
   giveType,
   makeSymbol,
   toTerm,
   makeTerm,
   toString,
   toEnd,
   term'
   )where
import Data.List
import DataType
-- for #3
-- first parameter is a bound on complexity of terms
term :: Int -> Signature -> [Term]
term n (Signature xs) = take n (nub (c ++ term' n (Signature xs) d))                    
                        where a = checkElement xs
                              b = xs \\ a 
                              c = makeSingleTerms a
                              d = c ++ makeTerm xs b c 

-- maybe also for terms of a specific type
termsOfType :: Int -> Type -> Signature -> [Term]
termsOfType n t (Signature xs) = if null a 
                                 then []
                                 else checkType t (term n (Signature xs)) xs  
                                 where a = term n (Signature xs)

-- can tansform [Term] in more readable forms
printTerm :: Int -> Signature -> [String]
printTerm n xs = nub (map toEnd (term n xs))

printTermsOfType :: Int -> Type -> Signature -> [String]
printTermsOfType n t xs = nub (map toEnd (termsOfType n t xs))

checkElement :: [FunctionSymbol] -> [FunctionSymbol]
checkElement [] = []
checkElement (FunctionSymbol x xs t:ys)
   | null xs = (FunctionSymbol x xs t) : checkElement ys
   | otherwise = [] ++ checkElement ys

makeSingleTerms :: [FunctionSymbol] -> [Term]
makeSingleTerms [] = []
makeSingleTerms (FunctionSymbol x _ _:ys) = Term x : makeSingleTerms ys

checkType :: Type -> [Term] -> [FunctionSymbol] -> [Term]
checkType _ [] _ = []
checkType a (x:xs) ys
   | a == giveType x ys = x : checkType a xs ys
   | otherwise = checkType a xs ys

giveType :: Term -> [FunctionSymbol] -> Type
giveType (Term _) [] = (Type "error")
giveType (Term a) (FunctionSymbol b _ t : ys)
   | a == b = t 
   | otherwise =  giveType (Term a) ys
giveType (Function _ _) [] = (Type "error")
giveType (Function a s) (FunctionSymbol b _ t : ys)
   | a == b = t 
   | otherwise =  giveType (Function a s) ys

makeSymbol :: [FunctionSymbol] -> [Term] -> FunctionSymbol -> [[Term]]
makeSymbol _ _ (FunctionSymbol _ [] _) = []
makeSymbol w s (FunctionSymbol a (x:xs) y) = checkType x s w : makeSymbol w s (FunctionSymbol a xs y)

toTerm :: String -> [[Term]] -> [Term]
toTerm _ [] = []
toTerm s (x:xs) 
   | null xs = [Function s x] 
   | otherwise = Function s x : toTerm s xs

makeTerm :: [FunctionSymbol] -> [FunctionSymbol] -> [Term] -> [Term]
makeTerm _ [] _ = []
makeTerm w (FunctionSymbol a x y : xs) t = toTerm a (sequence(makeSymbol w t (FunctionSymbol a x y))) ++ makeTerm w xs t

toString :: [FunctionSymbol] -> [String]
toString [] = []
toString (FunctionSymbol x _ _ :ys) = x : toString ys

toEnd :: Term -> String
toEnd (Term x) = x
toEnd (Function s xs) = s ++ "(" ++ intercalate "," (map toEnd xs) ++ ")"

term' :: Int -> Signature -> [Term] -> [Term]
term' n (Signature xs) w = if null (nub (makeTerm xs b w) \\ w) || length (nub (makeTerm xs b w)) >= n
                           then nub (makeTerm xs b w)
                           else term' n (Signature xs) (w ++ (nub (makeTerm xs b w) \\ w))
                           where a = checkElement xs
                                 b = xs \\ a 

