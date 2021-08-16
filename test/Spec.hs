import Test.Hspec
import Test.Hspec.QuickCheck

import DataType
import ArbitrarySig
import AllTerm
import InvalidTerm
import ValidTerm
import Test.QuickCheck
import Examples.Signatures
import Data.Maybe (isJust,fromJust)
import Data.List ((\\))


main :: IO ()
main = hspec $ do
  -- test cases (in the simplest form) have the following structure:
  -- specify "some description of the test case" <expression of type Bool>
  specify "a is a term that can be built from signature1" $
    Term "a" [] `elem` term 10 signature1
  specify "d is not a term that can be built from signature1" $
    Term "d" [] `notElem` term 10 signature1
  specify "h(x,f,g) is not a term that can be built from signature3" $
    Term "h" [Term "x" [],Term "f" [],Term "g" []] `notElem` term 10 signature1
  specify "terms computed from a signature are valid (for signature2 and size up to 30)" $
    all (isValid signature2) (term 30 signature2)
  specify "terms computed from a signature are valid (for signature4 and size up to 40)" $
    all (isValid signature4) (term 40 signature4)
  specify "terms computed from a signature are valid (for signature6 and size up to 20)" $
    all (isValid signature6) (term 20 signature6)
  specify "x(a,b,d) is not a term that can be built from signature4" $
    Term "x" [Term "a" [],Term "b" [],Term "d" []] `notElem` term 10 signature4
  specify "y(b) is not a term that can be built from signature4" $
    Term "y" [Term "b" []] `notElem` term 10 signature4
  specify "c is not a term that can be built from signature5" $
    Term "c" [] `notElem` term 10 signature5
  specify "b(e(d(c(b(a))))) is a term that can be built from signature5" $
    Term "b" [Term "e" [Term "d" [Term "c"[Term "b" [Term "a" []]]]]] `elem` term 10 signature5
  specify "b(e(d(c(a)))) is not a term that can be built from signature5" $
    Term "b" [Term "e" [Term "d" [Term "c" [Term "a" []]]]] `notElem` term 10 signature5
  specify "x(a,b,d) is an invalid term of signature4" $
    not (isValid signature4 (Term "x" [Term "a" [],Term "b" [],Term "d" []]))
  specify "t(b,u(x(a,b,x(a,b)))) is an invalid term of signature1" $
    not (isValid signature1 (Term "t" [Term "b" [],Term "u" [Term "x" [Term "a" [],Term "b" [],Term "x" [Term "a" [],Term "b" []]]]]))
  specify "g(x,f) is an invalid term of signature3" $
    not (isValid signature3 (Term "g" [Term "x" [],Term "f" []]))
  specify "f(a,b) is an invalid term of signature3" $
    not (isValid signature3 (Term "f" [Term "x" [],Term "y" []]))
  specify "f(e(a)) is an invalid term of signature2" $
    not (isValid signature2 (Term "f" [Term "e" [Term "a" []]]))
  specify "f(e) is a valid term of signature2" $
    isValid signature2 (Term "f" [Term "e" []])
  specify "x is an invalid term of signature4" $
    not (isValid signature4 (Term "x" []))
  specify "c(a,b,x) is an invalid term of signature6" $
    not (isValid signature6 (Term "c" [Term "a" [],Term "b" [],Term "x" []]))
  specify "t(a,b) is not a term of Type D that can be built from signature1" $
    Term "t" [Term "a" [],Term "b" []] `notElem` termsOfType 20 (Type "D") signature1
  specify "g(f(e),a) is a term of Type E that can be built from signature2" $
    Term "g" [Term "f" [Term "e" []], Term "a" []] `elem` termsOfType 20 (Type "E") signature2
  specify "h(x,f(x,x),g(y,y)) is not a term of Type D that can be built from signature3" $
    Term "h" [Term "x" [],Term "f" [Term "x" [],Term "x" []],Term "g" [Term "y" [],Term "y" []]] `notElem` termsOfType 20 (Type "D") signature3
  specify "h(x(a,b,c)) is not a term of Type F that can be built from signature4" $
    Term "h" [Term "x" [Term "a" [],Term "b" [],Term "c" []]] `notElem` termsOfType 20 (Type "F") signature4
  specify "b(e(d(c(b(a))))) is a term of Type B that can be built from signature5" $
    Term "b" [Term "e" [Term "d" [Term "c"[Term "b" [Term "a" []]]]]] `elem` termsOfType 20 (Type "B") signature5
  specify "c(a,b,f) is a term of Type B that can be built from signature6" $
    Term "c" [Term "a" [],Term "b" [],Term "f" []] `elem` termsOfType 20 (Type "B") signature6
  prop "check oneValidTerm (signature1)" $
    forAll (oneValidTerm signature1 (Just "t") 1 8) (\x -> isJust x ==> isValid signature1 (fromJust x))
  prop "check oneValidTerm (signature2)" $
    forAll (oneValidTerm signature2 (Just "e") 1 6) (\x -> isJust x ==> isValid signature2 (fromJust x))
  prop "check oneValidTerm (signature3)" $
    forAll (oneValidTerm signature3 (Just "f") 3 8) (\x -> isJust x ==> isValid signature3 (fromJust x))
  prop "check oneValidTerm (signature4)" $
    forAll (oneValidTerm signature4 Nothing 3 8) (\x -> isJust x ==> isValid signature4 (fromJust x))
  prop "check oneValidTerm (signature5)" $
    forAll (oneValidTerm signature5 (Just "b") 1 8) (\x -> isJust x ==> isValid signature5 (fromJust x))
  prop "check oneValidTerm (signature6)" $
    forAll (oneValidTerm signature6 (Just "a") 1 4) (\x -> isJust x ==> isValid signature6 (fromJust x))

  prop "Every term that can be generated with term should also be generatable with validTerms(signature1)" $
    forAll (elements (term 15 signature1)) (\x -> x `elem` validTerms signature1 Nothing (size x) (size x))
  prop "Every term that can be generated with term should also be generatable with validTerms(signature2)" $
    forAll (elements (term 10 signature2)) (\x -> x `elem` validTerms signature2 Nothing (size x) (size x))
  prop "Every term that can be generated with term should also be generatable with validTerms(signature3)" $
    forAll (elements (term 20 signature3)) (\x -> x `elem` validTerms signature3 Nothing (size x) (size x))
  prop "Every term that can be generated with term should also be generatable with validTerms(signature4)" $
    forAll (elements (term 10 signature4)) (\x -> x `elem` validTerms signature4 Nothing (size x) (size x))
  prop "Every term that can be generated with term should also be generatable with validTerms(signature5)" $
    forAll (elements (term 30 signature5)) (\x -> x `elem` validTerms signature5 Nothing (size x) (size x))
  prop "Every term that can be generated with term should also be generatable with validTerms(signature6)" $
    forAll (elements (term 15 signature6)) (\x -> x `elem` validTerms signature6 Nothing (size x) (size x))

  prop "Every term that can be generated with validTerms should also be generatable with term (signature1)" $
    forAll (elements(validTerms signature1 Nothing 1 8)) (\x -> x `elem` concatMap (`term` signature1) [1..])
  prop "Every term that can be generated with validTerms should also be generatable with term (signature2)" $
    forAll (elements(validTerms signature2 Nothing 3 10)) (\x -> x `elem` concatMap (`term` signature2) [1..])
  prop "Every term that can be generated with validTerms should also be generatable with term (signature3)" $
    forAll (elements(validTerms signature3 Nothing 4 9)) (\x -> x `elem` concatMap (`term` signature3) [1..])
  prop "Every term that can be generated with validTerms should also be generatable with term (signature4)" $
    forAll (elements(validTerms signature4 Nothing 1 4)) (\x -> x `elem` concatMap (`term` signature4) [1..])
  prop "Every term that can be generated with validTerms should also be generatable with term (signature5)" $
    forAll (elements(validTerms signature5 Nothing 4 4)) (\x -> x `elem` concatMap (`term` signature5) [1..])
  prop "Every term that can be generated with validTerms should also be generatable with term (signature6)" $
    forAll (elements(validTerms signature6 Nothing 2 10)) (\x -> x `elem` concatMap (`term` signature6) [1..])

  specify "The size of generated terms is really in the range (signature1)" $
    all (\t -> between (size t) 1 8) (validTerms signature1 Nothing 1 8)
  specify "The size of generated terms is really in the range (signature2)" $
    all (\t -> between (size t) 3 10) (validTerms signature2 Nothing 3 10)
  specify "The size of generated terms is really in the range (signature3)" $
    all (\t -> between (size t) 4 9) (validTerms signature3 Nothing 4 9)
  specify "The size of generated terms is really in the range (signature4)" $
    all (\t -> between (size t) 1 4) (validTerms signature4 Nothing 1 4)
  specify "The size of generated terms is really in the range (signature5)" $
    all (\t -> between (size t) 4 4) (validTerms signature5 Nothing 4 4)
  specify "The size of generated terms is really in the range (signature6)" $
    all (\t -> between (size t) 2 10) (validTerms signature6 Nothing 2 10)

  specify "The size of generated terms is really in the range (signature1)" $
    all (\t -> between (size t) 1 8) (validTerms signature1 (Just "t") 1 8)
  specify "The size of generated terms is really in the range (signature2)" $
    all (\t -> between (size t) 3 10) (validTerms signature2 (Just "b") 3 10)
  specify "The size of generated terms is really in the range (signature3)" $
    all (\t -> between (size t) 4 9) (validTerms signature3 (Just "h") 4 9)
  specify "The size of generated terms is really in the range (signature4)" $
    all (\t -> between (size t) 1 4) (validTerms signature4 (Just "z") 1 4)
  specify "The size of generated terms is really in the range (signature5)" $
    all (\t -> between (size t) 4 4) (validTerms signature5 (Just "c") 4 4)
  specify "The size of generated terms is really in the range (signature6)" $
    all (\t -> between (size t) 2 10) (validTerms signature6 (Just "e") 2 10)

  specify "A certain symbol contains exactly once (signature1)" $
    all (isOnce "t" . termSymbols) (validTerms signature1 (Just "t") 1 8)
  specify "A certain symbol contains exactly once (signature2)" $
    all (isOnce "b" . termSymbols) (validTerms signature2 (Just "b") 3 10)
  specify "A certain symbol contains exactly once (signature3)" $
    all (isOnce "h" . termSymbols) (validTerms signature3 (Just "h") 4 9)
  specify "A certain symbol contains exactly once (signature4)" $
    all (isOnce "z" . termSymbols) (validTerms signature4 (Just "z") 1 4)
  specify "A certain symbol contains exactly once (signature5)" $
    all (isOnce "c" . termSymbols) (validTerms signature5 (Just "c") 4 4)
  specify "A certain symbol contains exactly once (signature6)" $
    all (isOnce "e". termSymbols) (validTerms signature6 (Just "e") 2 10)
  specify "c(b,a,d) can be generated by signature6" $
    Term "c" [Term "b" [],Term "a" [],Term "d" []] `elem` validTerms signature6 (Just "a") 1 4
  specify "c(b,a,e) can be generated by signature6" $
    Term "c" [Term "b" [],Term "a" [],Term "e" []] `elem` validTerms signature6 (Just "a") 1 4
  specify "c(b,a,f) can be generated by signature6" $
    Term "c" [Term "b" [],Term "a" [],Term "f" []] `elem` validTerms signature6 (Just "a") 1 4

  prop "Eyery term that generated by invalidTerms is invalid (signature1)" $
    forAll (invalidTerms signature1 [(3,ONEMORE),(2,TYPE)] 1 20 >>= elements) (\x -> True `notElem` (map (isValid signature1) x))
  prop "Eyery term that generated by invalidTerms is invalid (signature2)" $
    forAll (invalidTerms signature2 [(2,ONELESS),(2,SYMBOLTYPE)] 1 10 >>= elements) (\x -> True `notElem` (map (isValid signature2) x))
  prop "Eyery term that generated by invalidTerms is invalid (signature3)" $
    forAll (invalidTerms signature3 [(1,SYMBOL),(3,SYMBOLTYPE)] 1 10 >>= elements) (\x -> True `notElem` (map (isValid signature3) x))
  prop "Eyery term that generated by invalidTerms is invalid (signature4)" $
    forAll (invalidTerms signature4 [(1,TYPE),(1,ONELESS)] 1 10 >>= elements) (\x -> True `notElem` (map (isValid signature4) x))
  prop "Eyery term that generated by invalidTerms is invalid (signature5)" $
    forAll (invalidTerms signature5 [(2,TYPE)] 1 10 >>= elements) (\x -> True `notElem` (map (isValid signature5) x))
  prop "Eyery term that generated by invalidTerms is invalid (signature6)" $
    forAll (invalidTerms signature6 [(3,SYMBOL),(3,SWAP)] 1 10 >>= elements) (\x -> True `notElem` (map (isValid signature6) x))

size :: Term -> Int
size fs = 1 + size' (#arguments fs)

size' :: [Term] -> Int
size' [] = 0
size' (f:fs)
  | null (#arguments f) = 1 + size' fs
  | otherwise = 1 + size' (#arguments f) + size' fs

between :: Int -> Int -> Int -> Bool
between n a b = n >= a && n <= b

isOnce :: String -> [String] -> Bool
isOnce s ls = length (filter (== s) ls) == 1

-- maybe also for terms of a specific type
termsOfType :: Int -> Type -> Signature -> [Term]
termsOfType n t sig = sameTypeTerms sig (term n sig) t

-- ComputeTerm
term :: Int -> Signature -> [Term]
term n sig = take n (constant ++ subTerms n sig constant)
                  where conSymbol = map #symbol (allConstants sig)
                        constant = map (`Term` []) conSymbol

sameTypeTerms :: Signature -> [Term] -> Type -> [Term]
sameTypeTerms sig ts t = filter (\x -> t == fromJust(theType sig (#symbol x))) ts

diffTypeTerms :: Signature -> [Term] -> [Type] -> [[Term]]
diffTypeTerms sig ts = map (sameTypeTerms sig ts)

theTerms :: Signature -> [Symbol] -> [Term] -> [Term]
theTerms sig fs ts = concatMap (\x -> map (Term (#symbol x)) (sequence(diffTypeTerms sig ts (#arguments x)))) fs

subTerms :: Int -> Signature -> [Term] -> [Term]
subTerms n sig w = if null (getAllT \\ w) || length getAllT >= n
                      then getAllT
                      else subTerms n sig (w ++ (getAllT \\ w))
                         where func = allFunctions sig
                               getAllT = theTerms sig func w

-- ValidCheck
isValid :: Signature -> Term -> Bool
isValid sig t = all (`elem` allSymbols sig) (termSymbols t) && isValidType t sig

isValidType :: Term -> Signature -> Bool
isValidType (Term s xs) w = isValidType' (fromJust (theArgumentsTypes w s)) xs w

isValidType' :: [Type] ->[Term] -> Signature -> Bool
isValidType' [] [] _ = True
isValidType' [] xs _ = null xs
isValidType' xs [] _ = null xs
isValidType' (t:ts) (Term s []:xs) w = isValidType' ts xs w && theType w s == Just t && s `elem` map #symbol (allConstants w)
isValidType' (t:ts) (Term s x':xs) w = isValidType (Term s x') w && isValidType' ts xs w && theType w s == Just t




