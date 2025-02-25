import Test.Hspec
import Test.Hspec.QuickCheck

import DataType
import AllTerm
import InvalidTerm
import ValidTerm
import Test.QuickCheck
import Examples.Signatures
import Examples.Functions
import Examples.ComputeTerm
import Examples.ValidCheck
import Data.Maybe (isJust,fromJust)


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
    forAll (oneValidTerm signature1 (Just "t") 1 8 Nothing `suchThat` isJust) (isValid signature1 . fromJust)
  prop "check oneValidTerm (signature2)" $
    forAll (oneValidTerm signature2 (Just "e") 1 6 Nothing `suchThat` isJust) (isValid signature2 . fromJust)
  prop "check oneValidTerm (signature3)" $
    forAll (oneValidTerm signature3 (Just "f") 3 8 Nothing `suchThat` isJust) (isValid signature3 . fromJust)
  prop "check oneValidTerm (signature4)" $
    forAll (oneValidTerm signature4 Nothing 3 8 Nothing `suchThat` isJust) (isValid signature4 . fromJust)
  prop "check oneValidTerm (signature5)" $
    forAll (oneValidTerm signature5 (Just "b") 1 8 Nothing `suchThat` isJust) (isValid signature5 . fromJust)
  prop "check oneValidTerm (signature6)" $
    forAll (oneValidTerm signature6 (Just "a") 1 4 Nothing `suchThat` isJust) (isValid signature6 . fromJust)

  prop "Every term that can be generated with term should also be generable with validTerms(signature1)" $
    all (\x -> x `elem` validTerms signature1 Nothing (size x) (size x) Nothing) (term 125 signature1)
  prop "Every term that can be generated with term should also be generable with validTerms(signature2)" $
    all (\x -> x `elem` validTerms signature2 Nothing (size x) (size x) Nothing) (term 100 signature2)
  prop "Every term that can be generated with term should also be generable with validTerms(signature3)" $
    all (\x -> x `elem` validTerms signature3 Nothing (size x) (size x) Nothing) (term 150 signature3)
  prop "Every term that can be generated with term should also be generable with validTerms(signature4)" $
    all (\x -> x `elem` validTerms signature4 Nothing (size x) (size x) Nothing) (term 100 signature4)
  prop "Every term that can be generated with term should also be generable with validTerms(signature5)" $
    all (\x -> x `elem` validTerms signature5 Nothing (size x) (size x) Nothing) (term 200 signature5)
  prop "Every term that can be generated with term should also be generable with validTerms(signature6)" $
    all (\x -> x `elem` validTerms signature6 Nothing (size x) (size x) Nothing) (term 125 signature6)

  prop "Every term containing 'a' exactly once should be generable with respective validTerms-call (signature1)" $
    all (\x -> x `elem` validTerms signature1 (Just "a") (size x) (size x) Nothing) (filter (isOnce "a" . termSymbols) $ term 125 signature1)
  prop "Every term containing 'b' exactly once should be generable with respective validTerms-call (signature2)" $
    all (\x -> x `elem` validTerms signature2 (Just "b") (size x) (size x) Nothing) (filter (isOnce "b" . termSymbols) $ term 100 signature2)
  prop "Every term containing 'h' exactly once should be generable with respective validTerms-call (signature3)" $
    all (\x -> x `elem` validTerms signature3 (Just "h") (size x) (size x) Nothing) (filter (isOnce "h" . termSymbols) $ term 150 signature3)
  prop "Every term containing 'd' exactly once should be generable with respective validTerms-call (signature4)" $
    all (\x -> x `elem` validTerms signature4 (Just "d") (size x) (size x) Nothing) (filter (isOnce "d" . termSymbols) $ term 100 signature4)
  prop "Every term containing 'c' exactly once should be generable with respective validTerms-call (signature5)" $
    all (\x -> x `elem` validTerms signature5 (Just "c") (size x) (size x) Nothing) (filter (isOnce "c" . termSymbols) $ term 200 signature5)
  prop "Every term containing 'c' exactly once should be generable with respective validTerms-call (signature6)" $
    all (\x -> x `elem` validTerms signature6 (Just "c") (size x) (size x) Nothing) (filter (isOnce "c" . termSymbols) $ term 125 signature6)

  prop "Every term that can be generated with validTerms should also be generable with term (signature1)" $
    all (\x -> x `elem` concatMap (`term` signature1) [1..]) (validTerms signature1 Nothing 1 8 Nothing)
  prop "Every term that can be generated with validTerms should also be generable with term (signature2)" $
    all (\x -> x `elem` concatMap (`term` signature2) [1..]) (validTerms signature2 Nothing 3 10 Nothing)
  prop "Every term that can be generated with validTerms should also be generable with term (signature3)" $
    all (\x -> x `elem` concatMap (`term` signature3) [1..]) (validTerms signature3 Nothing 4 9 Nothing)
  prop "Every term that can be generated with validTerms should also be generable with term (signature4)" $
    all (\x -> x `elem` concatMap (`term` signature4) [1..]) (validTerms signature4 Nothing 1 4 Nothing)
  prop "Every term that can be generated with validTerms should also be generable with term (signature5)" $
    all (\x -> x `elem` concatMap (`term` signature5) [1..]) (validTerms signature5 Nothing 4 4 Nothing)
  prop "Every term that can be generated with validTerms should also be generable with term (signature6)" $
    all (\x -> x `elem` concatMap (`term` signature6) [1..]) (validTerms signature6 Nothing 2 10 Nothing)

  specify "The size of generated terms is really in the range (signature1)" $
    all (\t -> between (size t) 1 8) (validTerms signature1 Nothing 1 8 Nothing)
  specify "The size of generated terms is really in the range (signature2)" $
    all (\t -> between (size t) 3 10) (validTerms signature2 Nothing 3 10 Nothing)
  specify "The size of generated terms is really in the range (signature3)" $
    all (\t -> between (size t) 4 9) (validTerms signature3 Nothing 4 9 Nothing)
  specify "The size of generated terms is really in the range (signature4)" $
    all (\t -> between (size t) 1 4) (validTerms signature4 Nothing 1 4 Nothing)
  specify "The size of generated terms is really in the range (signature5)" $
    all (\t -> between (size t) 4 4) (validTerms signature5 Nothing 4 4 Nothing)
  specify "The size of generated terms is really in the range (signature6)" $
    all (\t -> between (size t) 2 10) (validTerms signature6 Nothing 2 10 Nothing)

  specify "The size of generated terms is really in the range (signature1)" $
    all (\t -> between (size t) 1 8) (validTerms signature1 (Just "t") 1 8 Nothing)
  specify "The size of generated terms is really in the range (signature2)" $
    all (\t -> between (size t) 3 10) (validTerms signature2 (Just "b") 3 10 Nothing)
  specify "The size of generated terms is really in the range (signature3)" $
    all (\t -> between (size t) 4 9) (validTerms signature3 (Just "h") 4 9 Nothing)
  specify "The size of generated terms is really in the range (signature4)" $
    all (\t -> between (size t) 1 4) (validTerms signature4 (Just "z") 1 4 Nothing)
  specify "The size of generated terms is really in the range (signature5)" $
    all (\t -> between (size t) 4 4) (validTerms signature5 (Just "c") 4 4 Nothing)
  specify "The size of generated terms is really in the range (signature6)" $
    all (\t -> between (size t) 2 10) (validTerms signature6 (Just "e") 2 10 Nothing)

  specify "A certain symbol contains exactly once (signature1)" $
    all (isOnce "t" . termSymbols) (validTerms signature1 (Just "t") 1 8 Nothing)
  specify "A certain symbol contains exactly once (signature2)" $
    all (isOnce "b" . termSymbols) (validTerms signature2 (Just "b") 3 10 Nothing)
  specify "A certain symbol contains exactly once (signature3)" $
    all (isOnce "h" . termSymbols) (validTerms signature3 (Just "h") 4 9 Nothing)
  specify "A certain symbol contains exactly once (signature4)" $
    all (isOnce "z" . termSymbols) (validTerms signature4 (Just "z") 1 4 Nothing)
  specify "A certain symbol contains exactly once (signature5)" $
    all (isOnce "c" . termSymbols) (validTerms signature5 (Just "c") 4 4 Nothing)
  specify "A certain symbol contains exactly once (signature6)" $
    all (isOnce "e". termSymbols) (validTerms signature6 (Just "e") 2 10 Nothing)
  specify "c(b,a,d) can be generated by signature6" $
    Term "c" [Term "b" [],Term "a" [],Term "d" []] `elem` validTerms signature6 (Just "a") 1 4 Nothing
  specify "c(b,a,e) can be generated by signature6" $
    Term "c" [Term "b" [],Term "a" [],Term "e" []] `elem` validTerms signature6 (Just "a") 1 4 Nothing
  specify "c(b,a,f) can be generated by signature6" $
    Term "c" [Term "b" [],Term "a" [],Term "f" []] `elem` validTerms signature6 (Just "a") 1 4 Nothing

  prop "Eyery term that generated by invalidTerms is invalid (signature1)" $
    forAll (invalidTerms signature1 1 20 Nothing [(3,OneMore), (2,TypeChange)] >>= elements) (\x -> True `notElem` map (isValid signature1) x)
  prop "Eyery term that generated by invalidTerms is invalid (signature2)" $
    forAll (invalidTerms signature2 1 10 Nothing [(2,OneLess), (2,UnknownSymbol)] >>= elements) (\x -> True `notElem` map (isValid signature2) x)
  prop "Eyery term that generated by invalidTerms is invalid (signature3)" $
    forAll (invalidTerms signature3 1 10 Nothing [(1,NameTypo), (3,UnknownSymbol)] >>= elements) (\x -> True `notElem` map (isValid signature3) x)
  prop "Eyery term that generated by invalidTerms is invalid (signature4)" $
    forAll (invalidTerms signature4 1 10 Nothing [(1,TypeChange), (1,OneLess)] >>= elements) (\x -> True `notElem` map (isValid signature4) x)
  prop "Eyery term that generated by invalidTerms is invalid (signature5)" $
    forAll (invalidTerms signature5 1 10 Nothing [(2,TypeChange)] >>= elements) (\x -> True `notElem` map (isValid signature5) x)
  prop "Eyery term that generated by invalidTerms is invalid (signature6)" $
    forAll (invalidTerms signature6 1 10 Nothing [(3,NameTypo), (3,Swap)] >>= elements) (\x -> True `notElem` map (isValid signature6) x)

  prop "allTerms generates two list: one for valid one for invalid" $
    forAll (allTerms ["x","y","z","f","g","h"] [Type "A",Type "B",Type "C",Type "D"] [(1,TypeChange),(1,OneLess)] 4 1 10 5) (\x -> check x && check' x)
  prop "allTerms generates two list: one for valid one for invalid" $
    forAll (allTerms ["x","y","z","f","g","h"] [Type "A",Type "B",Type "C",Type "D"] [(3,NameTypo),(3,Swap)] 5 1 10 10) (\x -> check x && check' x)
  prop "allTerms generates two list: one for valid one for invalid" $
    forAll (allTerms ["x","y","z","f","g","h"] [Type "A",Type "B",Type "C",Type "D"] [(1,NameTypo),(3,UnknownSymbol)] 4 1 10 5) (\x -> check x && check' x)
  prop "allTerms generates two list: one for valid one for invalid" $
    forAll (allTerms ["a","b","c","d","e","f"] [Type "A",Type "B",Type "C"] [(1,TypeChange),(1,OneLess)] 4 1 10 5) (\x -> check x && check' x)
  prop "allTerms generates two list: one for valid one for invalid" $
    forAll (allTerms ["a","b","c","d","e","f"] [Type "A",Type "B",Type "C",Type "D"] [(3,NameTypo),(3,Swap)] 6 1 10 10) (\x -> check x && check' x)
  prop "allTerms generates two list: one for valid one for invalid" $
    forAll (allTerms ["a","b","c","d","e","f"] [Type "A",Type "B",Type "C",Type "D"] [(1,NameTypo),(3,UnknownSymbol)] 4 1 10 5) (\x -> check x && check' x)
  prop "allTerms generates two list: one for valid one for invalid" $
    forAll (allTerms ["a","b","c","d","e"] [Type "A",Type "B",Type "C",Type "D"] [(1,TypeChange),(1,OneLess)] 4 1 10 5) (\x -> check x && check' x)

check :: (Signature,([Term String],[[Term String]])) -> Bool
check (sig,(ts,_)) = all (isValid sig) ts

check' :: (Signature,([Term String],[[Term String]])) -> Bool
check' (sig,(_,ts)) = all (\x -> True `notElem` map (isValid sig) x) ts

size :: Term a -> Int
size = termSize

between :: Int -> Int -> Int -> Bool
between n a b = n >= a && n <= b

isOnce :: String -> [String] -> Bool
isOnce s ls = length (filter (== s) ls) == 1

-- maybe also for terms of a specific type
termsOfType :: Int -> Type -> Signature -> [Term String]
termsOfType n t sig = sameTypeTerms sig (term n sig) t
