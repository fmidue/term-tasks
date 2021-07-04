import Test.Hspec
import Test.Hspec.QuickCheck

import DataType
import ComputeTerm
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

  specify "c(a,b) is an invalid term (parameter length error) that can be built from signature6" $
    checkErrorType signature6 (Term "c" [Term "a" [],Term "b" []]) == (Just LENGTH)
  specify "z(d,y,c) is an invalid term (parameter order error) that can be built from signature4" $
    checkErrorType signature4 (Term "z" [Term "d" [],Term "y" [Term "b" [],Term "d" []],Term "c" []]) == (Just ORDER)
  specify "f(x,y) is an invalid term (parameter type error) that can be built from signature3" $
    checkErrorType signature3 (Term "f" [Term "x" [],Term "y" []]) == (Just TYPE)
  specify "g(a,f(e)) is an invalid term (parameter order error) that can be built from signature2" $
    checkErrorType signature2 (Term "g" [Term "a" [],Term "f" [Term "e" []]]) == (Just ORDER)
  specify "a(b) is an invalid term (parameter length error) that can be built from signature1" $
    checkErrorType signature1 (Term "a" [Term "b" []]) == (Just LENGTH)


  prop "randoming leads to invalid terms (for non constants) with invalidTerm (signature1)" $
    forAll (invalidTerm 5 LENGTH signature1>>= elements) (\t -> not (isValid signature1 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm (signature2)" $
    forAll (invalidTerm 5 TYPE signature2>>= elements) (\t -> not (isValid signature2 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm (signature3)" $
   forAll (invalidTerm 5 SYMBOL signature3>>= elements) (\t -> not (isValid signature3 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm (signature4)" $
    forAll (invalidTerm 5 ORDER signature4>>= elements) (\t -> not (isValid signature4 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm (signature5)" $
    forAll (invalidTerm 5 LENGTH signature5>>= elements) (\t -> not (isValid signature5 t) || isConstant t)

  prop "randoming leads to invalid terms (for non constants) with invalidTerm (signature1)" $
    forAll (invalidTerm 5 TYPE signature1) (all (\t -> not (isValid signature1 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with invalidTerm (signature6)" $
    forAll (invalidTerm 5 ORDER signature6) (all (\t -> not (isValid signature1 t) || isConstant t))

  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm (signature1)" $
    forAll (selectOneFunc signature1 >>= invalidTerm 10 LENGTH) (all (\t -> not (isValid signature1 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm (signature2)" $
    forAll (selectOneFunc signature2 >>= invalidTerm 10 ORDER) (all (\t -> not (isValid signature2 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm (signature3)" $
    forAll (selectOneFunc signature3 >>= invalidTerm 10 TYPE) (all (\t -> not (isValid signature3 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm (signature4)" $
    forAll (selectOneFunc signature4 >>= invalidTerm 10 SYMBOL) (all (\t -> not (isValid signature4 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm (signature5)" $
    forAll (selectOneFunc signature5 >>= invalidTerm 10 TYPE) (all (\t -> not (isValid signature5 t) || isConstant t))

isConstant :: Term -> Bool
isConstant (Term _ []) = True
isConstant _ = False

selectOneFunc :: Signature -> Gen Signature
selectOneFunc sig = do
  let con = getAllConstant sig
      f = getAllFunction sig
  fList <- elements f
  return (Signature (con++f))

-- maybe also for terms of a specific type
termsOfType :: Int -> Type -> Signature -> [Term]
termsOfType n t sig = getSameTypeTerm t (term n sig) sig

-- can tansform [Term] in more readable forms
printTerm :: Int -> Signature -> [String]
printTerm n xs = map transTerm (term n xs)

printTermsOfType :: Int -> Type -> Signature -> [String]
printTermsOfType n t xs = map transTerm (termsOfType n t xs)



