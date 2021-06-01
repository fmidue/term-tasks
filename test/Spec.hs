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
  specify "h(f,g) is not a term that can be built from signature3" $
    Term "h" [f3,g3] `notElem` term 10 signature1
  specify "terms computed from a signature are valid (for signature2 and size up to 30)" $
    all (isValid signature2) (term 30 signature2)
