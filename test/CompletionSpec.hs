module CompletionSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Completion
import Data.Char
import Data.List
import Data.Functor.Identity

spec :: Spec
spec = do

  context "Completion matching" $ do

    describe "Simple tests" $ do
      it "Simple substring" $
        complMatch "bar" "foo bar baz" `shouldBe` True
      it "Not a substring" $
        complMatch "quux" "foo bar baz" `shouldBe` False
      it "Case-insensitive" $
        complMatch "bAr" "foo BaR baz" `shouldBe` True
      it "Reversed" $
        complMatch "bar" "foo rab baz" `shouldBe` False
      it "Subsequence with holes" $
        complMatch "foo" "a f b o c o d" `shouldBe` True
      it "Correct chars, not enough" $
        complMatch "foo" "a f b o c d" `shouldBe` False

    prop "Empty strings never match" $ \str ->
      complMatch "" str `shouldBe` False

    prop "Subsequences match" $ forAll subSeqPairs $ \(sml, lrg) ->
      complMatch sml lrg `shouldBe` True

    prop "Non-subsequences don't match" $ \(xs, ys) ->
      not (map toLower xs `isSubsequenceOf` map toLower ys) ==>
      complMatch xs ys `shouldBe` False

  context "Completion suggestion" $ do

    let items = ["foo bar", "foo baz", "qoux"]
        compl = Compl (Identity <$> items) runIdentity
    it "Empty -> no suggestion" $
      complete compl "" `shouldBe` []
    it "'o' -> 3/3" $
      complete compl "o" `shouldBe` (Identity <$> items)
    it "'f ba' -> 2/3" $
      complete compl "f ba" `shouldBe` (Identity <$> ["foo bar", "foo baz"])
    it "'f bar' -> 1/3" $
      complete compl "f bar" `shouldBe` [Identity "foo bar"]
    it "'f barz' -> 0/3" $
      complete compl "f barz" `shouldBe` []
    
    describe "Arbitrary completion" $ do

      prop "Suggestions match" $ \aitems ->
        forAll (genShortSublists $ concat aitems) $ \str ->
          let suggestions = complete (Compl aitems id) str
          in  not (null suggestions) ==>
              forAll (elements suggestions) $ \sugg ->
                complMatch str sugg `shouldBe` True

      prop "Not-suggestions don't match" $ \aitems ->
        forAll (genShortSublists $ concat aitems) $ \str ->
          let nopes = aitems \\ complete (Compl aitems id) str
          in  not (null nopes) ==>
              forAll (elements nopes) $ \nope ->
                complMatch str nope `shouldBe` False

subSeqPairs :: Arbitrary a => Gen ([a], [a])
subSeqPairs = do
  xs <- arbitrary `suchThat` (not . all null)
  ys <- scale (*2) $ arbitrary
  return (concat xs, concat (il ys xs))
  where il [] xs          = xs
        il ys []          = ys
        il (x:xs) (y:ys)  = x : y : il xs ys

genShortSublists :: (Arbitrary a, Eq a) => [a] -> Gen [a]
genShortSublists xs = do
  len <- choose (1,2)
  vectorOf len (elements $ nub xs)
