module TestUtils where

import Test.Hspec

(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(===) = shouldBe