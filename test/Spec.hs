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
    Term "a" `elem` term 10 signature1
  specify "d is not a term that can be built from signature1" $
    Term "d" `notElem` term 10 signature1
  specify "h(x,f,g) is not a term that can be built from signature3" $
    Function "h" [Term "x",Function "f" [],Function "g" []] `notElem` term 10 signature1
  specify "terms computed from a signature are valid (for signature2 and size up to 30)" $
    all (isValid signature2) (term 30 signature2)
  specify "terms computed from a signature are valid (for signature4 and size up to 40)" $
    all (isValid signature4) (term 40 signature4)
  specify "x(a,b,d) is not a term that can be built from signature4" $
    Function "x" [Term "a",Term "b",Term "d"] `notElem` term 10 signature4
  specify "y(b) is not a term that can be built from signature4" $
    Function "y" [Term "b"] `notElem` term 10 signature4
  specify "c is not a term that can be built from signature5" $
    Term "c" `notElem` term 10 signature5
  specify "b(e(d(c(b(a))))) is a term that can be built from signature5" $
    Function "b" [Function "e" [Function "d" [Function "c"[Function "b" [Term "a"]]]]] `elem` term 10 signature5
  specify "b(e(d(c(a)))) is not a term that can be built from signature5" $
    Function "b" [Function "e" [Function "d" [Function "c" [Term "a"]]]] `notElem` term 10 signature5
  specify "x(a,b,d) is an invalid term of signature4" $
    isValid signature4 (Function "x" [Term "a",Term "b",Term "d"]) == False
  specify "t(b,u(x(a,b,x(a,b)))) is an invalid term of signature1" $
    isValid signature1 (Function "t" [Term "b",Function "u" [Function "x" [Term "a",Term "b",Function "x" [Term "a",Term "b"]]]])==False
  specify "g(x,f) is an invalid term of signature3" $
    isValid signature3 (Function "g" [Term "x",Function "f" []]) == False
  specify "f(a,b) is an invalid term of signature3" $
    isValid signature3 (Function "f" [Term "x",Term "y"]) == False


