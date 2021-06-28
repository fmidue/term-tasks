module ComputeTerm (
   term,
   termsOfType,
   printTerm,
   printTermsOfType,
   )where

import DataType
import GetSignatureInfo (getAllFunction,getAllConstantSymbol,giveType)
import Data.List ((\\),nub,intercalate)

-- for #3
-- first parameter is a bound on complexity of terms
term :: Int -> Signature -> [Term]
term n (Signature xs) = take n (nub (makeConstants (getAllConstantSymbol xs) ++ makeSubterm n (Signature xs) ft))
                        where constant = getAllConstantSymbol xs
                              ft = makeConstants constant ++ makeAllTerm xs (getAllFunction xs) (makeConstants constant)

-- maybe also for terms of a specific type
termsOfType :: Int -> Type -> Signature -> [Term]
termsOfType n t (Signature xs) = if null (term n (Signature xs))
                                 then []
                                 else getSameTypeTerm t (term n (Signature xs)) xs

-- can tansform [Term] in more readable forms
printTerm :: Int -> Signature -> [String]
printTerm n xs = nub (map transTerm (term n xs))

printTermsOfType :: Int -> Type -> Signature -> [String]
printTermsOfType n t xs = nub (map transTerm (termsOfType n t xs))

makeConstants :: [String] -> [Term]
makeConstants [] = []
makeConstants (x:xs) = Term x [] : makeConstants xs

getSameTypeTerm :: Type -> [Term] -> [FunctionSymbol] -> [Term]
getSameTypeTerm _ [] _ = []
getSameTypeTerm a (Term s t:xs) ys
   | Just a == giveType s ys = Term s t : getSameTypeTerm a xs ys
   | otherwise = getSameTypeTerm a xs ys

makeDiffTypeTermList :: [FunctionSymbol] -> [Term] -> FunctionSymbol -> [[Term]]
makeDiffTypeTermList _ _ (FunctionSymbol _ [] _) = []
makeDiffTypeTermList w s (FunctionSymbol a (x:xs) y) = getSameTypeTerm x s w : makeDiffTypeTermList w s (FunctionSymbol a xs y)

makeTerm :: String -> [[Term]] -> [Term]
makeTerm _ [] = []
makeTerm s (x:xs) = Term s x : makeTerm s xs

makeAllTerm :: [FunctionSymbol] -> [FunctionSymbol] -> [Term] -> [Term]
makeAllTerm _ [] _ = []
makeAllTerm w (FunctionSymbol a x y : xs) t = makeTerm a (sequence(makeDiffTypeTermList w t (FunctionSymbol a x y))) ++ makeAllTerm w xs t

transTerm :: Term -> String
transTerm (Term x []) = x
transTerm (Term s xs) = s ++ "(" ++ intercalate "," (map transTerm xs) ++ ")"

makeSubterm :: Int -> Signature -> [Term] -> [Term]
makeSubterm n (Signature xs) w = if null (nub (makeAllTerm xs subTerm w) \\ w) || length (nub (makeAllTerm xs subTerm w)) >= n
                                 then nub (makeAllTerm xs subTerm w)
                                 else makeSubterm n (Signature xs) (w ++ (nub (makeAllTerm xs subTerm w) \\ w))
                                    where subTerm = getAllFunction xs


