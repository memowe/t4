module T4.DataSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import T4.Data
import Data.Char
import Data.List
import Data.Text (unpack)
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP
import Data.Yaml
import Data.Time
import Data.Fixed
import qualified Data.Aeson as A

spec :: Spec
spec = do

  context "LocalTime wrapper" $ do
    let theTime     = SLT $ LocalTime (fromGregorian 2017 11 23)
                                      (TimeOfDay 17 42 37)
        iso8601Time = "2017-11-23 17:42:37"
    it "Simple constructor" $
      simpleLocalTime 2017 11 23 17 42 37 `shouldBe` theTime
    describe "JSON" $ do
      it "Correct simple JSONification" $
        toJSON theTime `shouldBe` String iso8601Time
      it "Correct simple parsing" $
        A.fromJSON (String iso8601Time) `shouldBe` A.Success theTime
      prop "SLT-JSON-SLT roundtrip" $ \slt ->
        let mslt = do String iso <- return $ toJSON (slt :: SimpleLocalTime)
                      readMaybe $ unpack iso
        in  mslt `shouldBe` Just slt

  context "Clock in/out data conversion" $ do
    let theTime = simpleLocalTime 2017 11 23 17 42 37
        cIn     = In theTime "foo" ["bar", "baz"]
        cOut    = Out theTime
    describe "Predicates" $ do
      it "in is in"       $ isIn  cIn   `shouldBe` True
      it "in is not out"  $ isOut cIn   `shouldBe` False
      it "out is out"     $ isOut cOut  `shouldBe` True
      it "out is not in"  $ isIn  cOut  `shouldBe` False
    it "Reading simple clock-in data" $
      decodeThrow "in:\n\
                  \  time: 2017-11-23 17:42:37\n\
                  \  category: foo\n\
                  \  tags:\n  - bar\n  - baz\n"
        `shouldBe` Just cIn
    it "Reading simple clock-out data" $
      decodeThrow "out:\n  time: 2017-11-23 17:42:37\n"
        `shouldBe` Just cOut
    prop "Read-write roundtrip" $ \clock ->
      let yaml = encode (clock :: Clock)
      in  decodeThrow yaml `shouldBe` Just clock

  context "Clock log groups" $ do
    let sameDay = (== 1) . length . group . map getDay
    prop "Grouping in days" $ \clockLog ->
      all sameDay (dayGroups clockLog)

  context "Core data aggregation" $ do

    describe "Categories" $ do
      prop "Clock categories in allCategories" $ \clocks ->
        let clockIns = filter isIn clocks
        in  not (null clockIns) ==>
            forAll (elements clockIns) $ \clock ->
              category clock `elem` allCategories clocks
      prop "allCategories in clocks" $ \clocks ->
        let categories = allCategories clocks
        in  not (null categories) ==>
            forAll (elements categories) $ \cat ->
              cat `elem` map category (filter isIn clocks)

    describe "Tags" $ do
      prop "Clock tags in allTags" $ \clocks ->
        let clockIns = filter isIn clocks
        in  not (null clockIns) ==>
            forAll (elements clockIns) $ \clock ->
              not (null $ tags clock) ==>
              forAll (elements $ tags clock) $ \tag ->
                tag `elem` allTags clocks
      prop "allTags in clocks" $ \clocks ->
        let theTags = allTags clocks
        in  not (null theTags) ==>
            forAll (elements theTags) $ \tag ->
              tag `elem` concatMap tags (filter isIn clocks)

instance Read SimpleLocalTime where
  readsPrec _ = readP_to_S $ do
    y <-              dig 4
    m <- char '-' >>  dig 2
    d <- char '-' >>  dig 2
    h <- char ' ' >>  dig 2
    i <- char ':' >>  dig 2
    s <- char ':' >>  dig 2
    return $ simpleLocalTime y m d h i s
    where dig n = read <$> count n (satisfy isDigit)

instance Arbitrary SimpleLocalTime where
  arbitrary = SLT . intSecs <$> arbitrary
    where intSecs lt@(LocalTime _ tod@(TimeOfDay _ _ s)) =
            lt {localTimeOfDay = tod {todSec = floor' s}}
          floor' = fromIntegral . (floor :: Pico -> Int)

instance Arbitrary Clock where
  arbitrary = oneof [ Out <$> arbitrary
                    , In  <$> arbitrary
                          <*> arbitrary `suchThat` valid
                          <*> listOf (arbitrary `suchThat` valid)
                    ]
    where valid = (&&) <$> ('#' `notElem`) <*> (',' `notElem`)
