module T4Spec where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do

  prop "Answer is 42" $ \question ->
    answer question `shouldBe` 42

  where answer :: String -> Int
        answer _ = 42
