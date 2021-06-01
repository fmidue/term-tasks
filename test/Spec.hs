import Test.Hspec

import DataType
import ComputeTerm
import ValidCheck

import Examples.Signatures

main :: IO ()
main = hspec $ do
  -- test cases (in the simplest form) have the following structure:
  -- specify "some description of the test case" <expression of type Bool>
  specify "a is a term that can be built from signature1" $
    Term "a" [] `elem` term 10 signature1
  specify "d is not a term that can be built from signature1" $
    Term "d" [] `notElem` term 10 signature1
  specify "h(f,g) is not a term that can be built from signature3" $
    Function "h" [Function "f" [Term "x" [],Term "x" []],Function "g" [Term "x" [],Term "y" []]] `notElem` term 10 signature1
  specify "terms computed from a signature are valid (for signature2 and size up to 30)" $
    all (isValid signature2) (term 30 signature2)
  specify "terms computed from a signature are valid (for signature4 and size up to 40)" $
    all (isValid signature4) (term 40 signature4)
  specify "x(a,b,d) is not a term that can be built from signature4" $
    Function "x" [Term "a" [],Term "b" [],Term "d" []] `notElem` term 10 signature4
  specify "y(b) is not a term that can be built from signature4" $
    Function "y" [Term "b" []] `notElem` term 10 signature4
  specify "c is not a term that can be built from signature5" $
    Term "c" [] `notElem` term 10 signature5
  specify "b(e(d(c(b(a))))) is a term that can be built from signature5" $
    Function "b" [Function "e" [Function "d" [Function "c"[Function "b" [Term "a" []]]]]] `elem` term 10 signature5
  specify "b(e(d(c(a)))) is not a term that can be built from signature5" $
    Function "b" [Function "e" [Function "d" [Function "c" [Term "a" []]]]] `notElem` term 10 signature5
