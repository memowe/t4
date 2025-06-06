module T4.ReportSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import T4.Data
import T4.Report
import T4.DataSpec () -- Arbitrary Clock instance
import Data.List
import Data.Function
import Data.Maybe
import Data.Map ((!))
import qualified Data.Map as M
import Data.Time

spec :: Spec
spec = do
  
  context "Durations reports" $ do

    describe "Example" $ do
      let slt     = simpleLocalTime
          clocks  = [ In  (slt 2017 11 23 16 42 23) (Just "c1") ["t1", "t2"]
                    , Out (slt 2017 11 23 17 42 23)
                    , In  (slt 2018 11 23 17 42 23) (Just "c2") ["t2"]
                    , In  (slt 2018 11 24 17 42 23) (Just "c3") ["t3"]
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
        let clocks  = [cx, cy]
            clock   = minimum clocks
        isIn clock && isJust (category clock) ==> do
          let cat = fromJust (category clock)
          M.keys (categoryDurations clocks) `shouldBe` [cat]
      prop "Tags extraction" $ \(cx, cy) -> do
        let clocks  = [cx, cy]
            clock   = minimum clocks
        isIn clock ==>
          M.keys (tagDurations clocks)
            `shouldMatchList` nub (tags clock)
      prop "Category duration extraction" $ \(cx, cy) -> do
        let clocks    = sort [cx, cy]
            (c1, c2)  = (head clocks, last clocks)
        isIn c1 && isJust (category c1) ==> do
          let cat   = fromJust (category c1)
              diff  = (diffLocalTime `on` getLocalTime . time) c2 c1
          categoryDurations clocks ! cat `shouldBe` diff
      prop "Tags duration extraction" $ \(cx, cy) -> do
        let clocks    = sort [cx, cy]
            (c1, c2)  = (head clocks, last clocks)
        isIn c1 && not (null $ tags c1) ==> do
          let diff  = (diffLocalTime `on` getLocalTime . time) c2 c1
              durs  = tagDurations clocks
          forAll (elements $ tags c1) $ \tag ->
            durs ! tag `shouldBe` diff
