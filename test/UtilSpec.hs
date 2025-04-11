module UtilSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import T4.Data
import T4.DataSpec () -- Arbitrary Clock instance
import Util
import Data.List
import Data.Function
import Data.Bifunctor
import Data.Map ((!))
import qualified Data.Map as M
import Data.Time

lt :: Int -> Int -> Int -> Int -> Int -> Int -> LocalTime
lt y m d h i s = getLocalTime $ simpleLocalTime y m d h i s

spec :: Spec
spec = do

  context "Duration helper function" $ do

    describe "Example" $ do
      let entries = [ (['1'],     lt 2017 7 17 15 42 0)
                    , (['2','3'], lt 2017 7 17 17 42 0)
                    , ([],        lt 2017 7 17 20 42 0)
                    , (['3','3'], lt 2018 7 17 17 42 0)
                    , ([],        lt 2021 7 17 14 42 0)
                    ]
      it "Correct intervall lengths" $
        M.toList (durations id entries) `shouldMatchList`
            [ ('1', secondsToNominalDiffTime (60*60*2))
            , ('2', secondsToNominalDiffTime (60*60*3))
            , ('3', secondsToNominalDiffTime (60*60*24*(365+365+366)))
            ]

    describe "No usable inputs" $ do
      it "Empty input -> empty output" $
        durations id ([] :: [([Int], LocalTime)])
          `shouldBe` M.empty
      prop "Singleton input -> empty output" $ \x ->
        durations id [x :: ([Char], LocalTime)] `shouldBe` M.empty

    prop "All the values" $ \xs -> not (null xs) ==> do
      let vals  = nub $ concatMap fst (init $ sortOn snd xs)
          dvals = M.keys $ durations id (xs :: [([Int], LocalTime)])
      not (null dvals) ==> dvals `shouldMatchList` vals

    prop "Durations are non-negative" $ \xs -> do
      let durs = M.elems $ durations id (xs :: [([Char], LocalTime)])
      not (null durs) ==> forAll (elements durs) (`shouldSatisfy` (>= 0))

    prop "Correct duration for single slots" $ \(x, y) -> do
      let clocks    = sortOn snd [x, y] :: [([Int], LocalTime)]
          (x1, x2)  = (head clocks, last clocks)
      not (null $ fst x1) ==> do
        let diff  = (diffLocalTime `on` snd) x2 x1
            durs  = durations id clocks
        forAll (elements $ M.keys durs) $ \val ->
          durs ! val `shouldBe` diff

    prop "Order doesn't matter" $ \xs -> do
      let durs = durations id (xs :: [([Int], LocalTime)])
      forAll (shuffle xs) $ \ys -> durations id ys `shouldBe` durs

  context "Difference time splitting" $ do

    describe "splitDiffTime" $ do
      let diff1   = secondsToNominalDiffTime 3702
          dconf   = DurConf [ DurUnit "s" "s" 60
                            , DurUnit "m" "m" 60
                            , DurUnit "h" "h" (fromIntegral (maxBound :: Int))
                            ]
          splits1 = splitDiffTime dconf diff1
      it "Correctly split 3700 seconds with custom duration config" $
        map (second short) splits1 `shouldBe` [(1,"h"), (1,"m"), (42,"s")]
      let diff2   = secondsToNominalDiffTime (60 * 60 * 17 + 42)
          splits2 = splitDiffTime naturalDurationConfig diff2
      it "Correctly split 17:00:42 in natural duration config" $
        map (second short) splits2
          `shouldBe` [(0,"y"), (0,"mo"), (0,"d"), (17,"h"), (0,"mi"), (42,"s")]
      let splits3 = splitDiffTime manDurationConfig diff2
      it "Correctly split 17:00:42 in man duration config" $
        map (second short) splits3
          `shouldBe` [(0,"y"), (0,"mo"), (2,"d"), (1,"h"), (0,"mi"), (42,"s")]
