module MyLibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import MyLib

spec :: Spec
spec = describe "MyLib tests" $

  describe "Welcome" $ do

    prop "Starts with welcome" $ \name ->
      welcome name `shouldStartWith` "Welcome, "

    prop "Ends with exclamation mark" $ \name ->
      welcome name `shouldEndWith` "!"

    prop "Contains name" $ \name ->
      welcome name `shouldContain` name
