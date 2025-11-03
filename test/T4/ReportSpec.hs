module T4.ReportSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import T4.Data
import T4.Report
import T4.DataSpec () -- Arbitrary Clock instance
import Data.Function
import Data.Maybe
import qualified Data.Set as S
import Data.Map ((!))
import qualified Data.Map as M
import Data.Time

spec :: Spec
spec = do
  
  context "Durations reports" $ do

    describe "Example" $ do
      let slt     = simpleLocalTime
          clocks  = S.fromList
            [ In  (slt 2017 11 23 16 42 23) (Just "c1") (S.fromList ["t1", "t2"])
            , Out (slt 2017 11 23 17 42 23)
            , In  (slt 2018 11 23 17 42 23) (Just "c2") (S.fromList ["t2"])
            , In  (slt 2018 11 24 17 42 23) (Just "c3") (S.fromList ["t3"])
            , Out (slt 2018 11 24 17 43 5)
            ]
      it "Correct category durations" $
        M.toList (categoryDurations clocks) `shouldMatchList`
          [ ("c1", secondsToNominalDiffTime (60*60))
          , ("c2", secondsToNominalDiffTime (60*60*24))
          , ("c3", secondsToNominalDiffTime 42)
          ]
      it "Correct tag durations" $
        M.toList (tagDurations clocks) `shouldMatchList`
          [ ("t1", secondsToNominalDiffTime (60*60))
          , ("t2", secondsToNominalDiffTime (60*60 + 60*60*24))
          , ("t3", secondsToNominalDiffTime 42)
          ]

    describe "Extraction functions" $ do
      prop "Category extraction" $ \(cx, cy) -> do
        let clocks  = S.fromList [cx, cy]
            clock   = minimum clocks
        isIn clock && isJust (category clock) ==> do
          let cat = fromJust (category clock)
          M.keysSet (categoryDurations clocks) `shouldBe` S.singleton cat
      prop "Tags extraction" $ \(cx, cy) -> do
        let clocks  = S.fromList [cx, cy]
            clock   = minimum clocks
        isIn clock ==>
          M.keysSet (tagDurations clocks) `shouldBe` tags clock
      prop "Category duration extraction" $ \(cx, cy) -> do
        let (c1, c2) = if cx <= cy then (cx, cy) else (cy, cx)
        isIn c1 && isJust (category c1) ==> do
          let cat   = fromJust (category c1)
              diff  = (diffLocalTime `on` getLocalTime . time) c2 c1
          categoryDurations (S.fromList [c1, c2]) ! cat `shouldBe` diff
      prop "Tags duration extraction" $ \(cx, cy) -> do
        let (c1, c2) = if cx <= cy then (cx, cy) else (cy, cx)
        isIn c1 && not (null $ tags c1) ==> do
          let diff  = (diffLocalTime `on` getLocalTime . time) c2 c1
              durs  = tagDurations $ S.fromList [c1, c2]
          forAll (elements $ S.toList $ tags c1) $ \tag ->
            durs ! tag `shouldBe` diff

    describe "Text reports" $ do
      let testDM = M.fromList [ ("foo", secondsToNominalDiffTime 2*60*60)
                              , ("bar", secondsToNominalDiffTime 8*60*60+42)]
      it "Basic text report" $
        showDurMap False False False testDM
          `shouldBe` ["bar: 1d 0h 0mi", "foo: 2h 0mi"]
      it "Sorted by duration" $
        showDurMap True False False testDM
          `shouldBe` ["foo: 2h 0mi", "bar: 1d 0h 0mi"]
      it "Natural time instead of man-days" $
        showDurMap True True False testDM
          `shouldBe` ["foo: 2h 0mi", "bar: 8h 0mi"]
      it "With seconds" $
        showDurMap True False True testDM
          `shouldBe` ["foo: 2h 0mi 0s", "bar: 1d 0h 0mi 42s"]
      prop "# Entries = # Lines" $ \dm ->
        length (showDurMap False False False dm)
          `shouldBe` M.size dm
