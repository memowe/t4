module CompletionSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Completion
import Data.Char
import Data.String ()
import Data.List hiding ((\\))
import Data.Set ((\\))
import qualified Data.Set as S
import Data.Functor.Identity
import qualified System.Console.Haskeline.Completion as HC

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

    prop "Subsequences match" $ forAll subSeqPairs $ \(sml, lrg) ->
      complMatch sml lrg `shouldBe` True

    prop "Non-subsequences don't match" $ \(xs, ys) ->
      not (map toLower xs `isSubsequenceOf` map toLower ys) ==>
      complMatch xs ys `shouldBe` False

  context "Completion suggestion" $ do

    let items = S.fromList ["foo bar", "foo baz", "qoux"]
        compl = Compl items id
    it "Empty -> all suggestions" $
      complete compl "" `shouldBe` items
    it "'o' -> 3/3" $
      complete compl "o" `shouldBe` items
    it "'f ba' -> 2/3" $
      complete compl "f ba"
        `shouldBe` S.fromList ["foo bar", "foo baz"]
    it "'f bar' -> 1/3" $
      complete compl "f bar"
        `shouldBe` S.singleton "foo bar"
    it "'f barz' -> 0/3" $
      complete compl "f barz" `shouldBe` S.empty

    describe "Arbitrary completion" $ do

      prop "Suggestions match" $ \aitems ->
        forAll (genShortSublists $ concat aitems) $ \str ->
          let suggestions = complete (Compl aitems id) str
          in  not (null suggestions) ==>
              forAll (elements $ S.toList suggestions) $ \sugg ->
                complMatch str sugg `shouldBe` True

      prop "Not-suggestions don't match" $ \aitems ->
        forAll (genShortSublists $ concat aitems) $ \str ->
          let nopes = aitems \\ complete (Compl aitems id) str
          in  not (null nopes) ==>
              forAll (elements $ S.toList nopes) $ \nope ->
                complMatch str nope `shouldBe` False

  context "Haskeline completion" $ do

    describe "Completion list generation" $ do

      it "Basic transformation" $
        haskelineCompletions (Compl (S.singleton "foo") id) "fo"
          `shouldBe` [HC.Completion "foo" "foo" True]

      prop "Replacement = Display" $
        forAll genMatchPairs $ \(compl, match) ->
          forAll (elements $ haskelineCompletions compl match) $ \hc ->
            HC.replacement hc `shouldBe` HC.display hc

      prop "Result is finished" $
        forAll genMatchPairs $ \(compl, match) ->
          forAll (elements $ haskelineCompletions compl match)
            HC.isFinished

      prop "Input matches display" $
        forAll genMatchPairs $ \(compl, match) ->
          forAll (elements $ haskelineCompletions compl match) $ \hc ->
            HC.display hc `shouldSatisfy` complMatch match

      prop "Same completion list" $
        forAll genMatchPairs $ \(compl, match) ->
          map HC.display (haskelineCompletions compl match)
            `shouldBe` S.toList (complete compl match)

    describe "Completion function transformation" $ do

      prop "Simple arbitrary completion" $
        forAll genMatchPairs $ \(compl, match) ->
          let complf = haskelineCompletionFunc compl
              result = complf (reverse match, "")
              compls = haskelineCompletions compl match
          in  runIdentity result `shouldBe` ("", compls)

      describe "Examples with word completion" $ do
        let compl     = Compl (S.fromList $ words "foo bar baz") id
            complf    = haskelineCompletionFunc compl
            hcompl w  = HC.Completion w w True
        it "First word" $ runIdentity (complf ("f", ""))
          `shouldBe` ("", [hcompl "foo"])
        it "Second word" $ runIdentity (complf ("ab oof", ""))
          `shouldBe` (" oof", hcompl <$> ["bar", "baz"])

subSeqPairs :: Arbitrary a => Gen ([a], [a])
subSeqPairs = do
  xs <- arbitrary `suchThat` (not . all null)
  ys <- scale (*2) arbitrary
  return (concat xs, concat (il ys xs))
  where il [] xs          = xs
        il ys []          = ys
        il (x:xs) (y:ys)  = x : y : il xs ys

genShortSublists :: (Arbitrary a, Eq a) => [a] -> Gen [a]
genShortSublists xs = do
  len <- choose (1,2)
  vectorOf len (elements $ nub xs)

genCompletions :: Gen (Completion String)
genCompletions = do
  ws <- listOf $ listOf $ arbitrary `suchThat` (not . isSpace)
  return $ Compl (S.fromList ws) id

genMatches :: Completion a -> Gen String
genMatches (Compl ws toString) = do
  str <- (toString <$> elements (S.toList ws)) `suchThat` (not . null)
  sublistOf str `suchThat` (not . null)

genMatchPairs :: Gen (Completion String, String)
genMatchPairs = do
  compl <- genCompletions `suchThat` (notEmpty . complItems)
  match <- genMatches compl
  return (compl, match)
  where notEmpty = (&&) <$> not . null <*> (not . any null)

instance Show (Completion a) where
  show (Compl items toString) =
    "Compl (complItems=" ++ show (toString <$> S.toList items) ++ ")"
