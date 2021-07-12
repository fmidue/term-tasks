import Test.Hspec
import Test.Hspec.QuickCheck

import DataType
import ComputeTerm (term,sameTypeTerms)
import ValidCheck
import ArbitrarySig
import ArbitraryTerm
import GetSignatureInfo
import DealWithTerm
import CheckError
import Test.QuickCheck
import Examples.Signatures


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




selectOneFunc :: Signature -> Gen Signature
selectOneFunc sig = do
  let con = allConstants sig
      f = allFunctions sig
  fList <- elements f
  return (Signature (con++f))

-- maybe also for terms of a specific type
termsOfType :: Int -> Type -> Signature -> [Term]
termsOfType n t sig = sameTypeTerms sig (term n sig) t

-- can tansform [Term] in more readable forms
printTerm :: Int -> Signature -> [String]
printTerm n xs = map termForm (term n xs)

printTermsOfType :: Int -> Type -> Signature -> [String]
printTermsOfType n t xs = map termForm (termsOfType n t xs)



