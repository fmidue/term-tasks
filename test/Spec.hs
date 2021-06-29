import Test.Hspec
import Test.Hspec.QuickCheck

import DataType
import ComputeTerm
import ValidCheck
import ArbitrarySig
import ArbitraryTerm
import GetSignatureInfo
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

  prop "randoming leads to invalid terms (for non constants) with invalidTerm1 (signature1)" $
    forAll (invalidTerm1 5 signature1 >>= elements) (\t -> not (isValid signature1 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm1 (signature2)" $
    forAll (invalidTerm1 5 signature2 >>= elements) (\t -> not (isValid signature2 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm1 (signature3)" $
   forAll (invalidTerm1 5 signature3 >>= elements) (\t -> not (isValid signature3 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm1 (signature4)" $
    forAll (invalidTerm1 5 signature4 >>= elements) (\t -> not (isValid signature4 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm1 (signature5)" $
    forAll (invalidTerm1 5 signature5 >>= elements) (\t -> not (isValid signature5 t) || isConstant t)

  prop "randoming leads to invalid terms (for non constants) with TermOfType2 (signature1)" $
   forAll (invalidTerm2 5 signature1 >>= elements) (\t -> not (isValid signature1 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm2 (signature2)" $
    forAll (invalidTerm2 5 signature2 >>= elements) (\t -> not (isValid signature2 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm2 (signature3)" $
    forAll (invalidTerm2 5 signature3 >>= elements) (\t -> not (isValid signature3 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm2 (signature4)" $
    forAll (invalidTerm2 5 signature4 >>= elements) (\t -> not (isValid signature4 t) || isConstant t)
  prop "randoming leads to invalid terms (for non constants) with invalidTerm2 (signature5)" $
    forAll (invalidTerm2 5 signature5 >>= elements) (\t -> not (isValid signature5 t) || isConstant t)

  prop "randoming leads to invalid terms (for non constants) with invalidTerm1 (signature1)" $
    forAll (invalidTerm1 5 signature1) (all (\t -> not (isValid signature1 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with invalidTerm1 (signature6)" $
    forAll (invalidTerm1 5 signature6) (all (\t -> not (isValid signature1 t) || isConstant t))

  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm1 (signature1)" $
    forAll (selectOneFunc signature1 >>= invalidTerm1 10) (all (\t -> not (isValid signature1 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm1 (signature2)" $
    forAll (selectOneFunc signature2 >>= invalidTerm1 10) (all (\t -> not (isValid signature2 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm1 (signature3)" $
    forAll (selectOneFunc signature3 >>= invalidTerm1 10) (all (\t -> not (isValid signature3 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm1 (signature4)" $
    forAll (selectOneFunc signature4 >>= invalidTerm1 10) (all (\t -> not (isValid signature4 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with totalinvalidTerm1 (signature5)" $
    forAll (selectOneFunc signature5 >>= invalidTerm1 10) (all (\t -> not (isValid signature5 t) || isConstant t))

  prop "randoming leads to invalid terms (for non constants) with invalidTerm2 (signature1)" $
    forAll (selectOneFunc signature1 >>= invalidTerm2 10) (all (\t -> not (isValid signature1 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with invalidTerm2 (signature2)" $
    forAll (selectOneFunc signature2 >>= invalidTerm2 10) (all (\t -> not (isValid signature2 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with invalidTerm2 (signature3)" $
    forAll (selectOneFunc signature3 >>= invalidTerm2 10) (all (\t -> not (isValid signature3 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with invalidTerm2 (signature4)" $
    forAll (selectOneFunc signature4 >>= invalidTerm2 10) (all (\t -> not (isValid signature4 t) || isConstant t))
  prop "randoming leads to invalid terms (for non constants) with invalidTerm2 (signature5)" $
    forAll (selectOneFunc signature5 >>= invalidTerm2 10) (all (\t -> not (isValid signature5 t) || isConstant t))

isConstant :: Term -> Bool
isConstant (Term _ []) = True
isConstant _ = False

selectOneFunc :: Signature -> Gen Signature
selectOneFunc sig = do
  let con = getAllConstant sig
      f = getAllFunction sig
  fList <- elements f
  return (Signature (con++f))

