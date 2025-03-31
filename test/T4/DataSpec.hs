{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module T4.DataSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import T4.Data
import Data.Char
import Data.List
import Data.Maybe
import Data.Text (unpack, pack)
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP
import Data.Yaml
import Data.Time
import Data.Fixed
import qualified Data.Aeson as A

spec :: Spec
spec = do

  context "LocalTime wrapper" $ do
    it "Simple constructor" $
      simpleLocalTime 2017 11 23 17 42 37
        `shouldBe` SLT (LocalTime (fromGregorian 2017 11 23)
                                  (TimeOfDay 17 42 37))
    prop "Ord instance based on LocalTime" $ \(slt1, slt2) ->
      slt1 <= slt2 `shouldBe` getLocalTime slt1 <= getLocalTime slt2
    describe "Stringification" $ do
      describe "Just the date" $ do
        it "Simple example" $
          let slt = simpleLocalTime 2017 11 23 17 42 37
          in  dateString slt `shouldBe` "2017-11-23"
        prop "General date extraction" $ \slt ->
          let day = localDay (getLocalTime slt)
              ymd = formatTime defaultTimeLocale "%Y-%m-%d" day
          in  dateString slt `shouldBe` ymd
      describe "Just the time" $ do
        it "Simple example" $
          let slt = simpleLocalTime 2017 11 23 17 42 37
          in  timeString slt `shouldBe` "17:42:37"
        prop "General time extraction" $ \slt ->
          let tme = localTimeOfDay (getLocalTime slt)
              hms = formatTime defaultTimeLocale "%H:%M:%S" tme
          in  timeString slt `shouldBe` hms
      describe "Full stringification" $ do
        it "Simple example" $
          let slt = simpleLocalTime 2017 11 23 17 42 37
          in  sltString slt `shouldBe` "2017-11-23 17:42:37"
        prop "General stringification" $ \slt ->
          sltString slt `shouldBe` dateString slt ++ " " ++ timeString slt
    describe "JSON" $ do
      let theTime     = SLT $ LocalTime (fromGregorian 2017 11 23)
                                        (TimeOfDay 17 42 37)
          iso8601Time = "2017-11-23 17:42:37"
      it "Correct simple JSONification" $
        toJSON theTime `shouldBe` String iso8601Time
      it "Correct simple parsing" $
        A.fromJSON (String iso8601Time) `shouldBe` A.Success theTime
      prop "JSONification contains stringification" $ \slt ->
        toJSON slt `shouldBe` String (pack $ sltString slt)
      prop "SLT-JSON-SLT roundtrip" $ \slt ->
        let mslt = do String iso <- return $ toJSON (slt :: SimpleLocalTime)
                      readMaybe $ unpack iso
        in  mslt `shouldBe` Just slt

  context "Clock in/out data" $ do
    let theTime   = simpleLocalTime 2017 11 23 17 42 37
        cIn       = In theTime (Just "foo") ["bar", "baz"]
        cInNoCat  = In theTime Nothing      ["bar", "baz"]
        cOut      = Out theTime

    describe "Predicates" $ do
      it "in is in"       $ isIn  cIn   `shouldBe` True
      it "in is not out"  $ isOut cIn   `shouldBe` False
      it "out is out"     $ isOut cOut  `shouldBe` True
      it "out is not in"  $ isIn  cOut  `shouldBe` False

    prop "Ord instance based on SLT" $ \(c1, c2) ->
      c1 <= c2 `shouldBe` time c1 <= time c2

    context "Summary: stringification for humans" $ do
      it "Simple clock in" $
        summary cIn `shouldBe` "in (2017-11-23 17:42:37) [foo] #bar #baz"
      it "Clock in without category" $
        summary cInNoCat `shouldBe` "in (2017-11-23 17:42:37) #bar #baz"
      it "Clock out" $
        summary cOut `shouldBe` "out (2017-11-23 17:42:37)"

    context "YAML clock data" $ do
      it "Reading simple clock-in data" $
        decodeThrow "in:\n\
                    \  time: 2017-11-23 17:42:37\n\
                    \  category: foo\n\
                    \  tags:\n  - bar\n  - baz\n"
          `shouldBe` Just cIn
      describe "Reading simple clock-in data without category" $ do
        it "Empty category property" $
          decodeThrow "in:\n\
                      \  time: 2017-11-23 17:42:37\n\
                      \  category: \n\
                      \  tags:\n  - bar\n  - baz\n"
            `shouldBe` Just cInNoCat
        it "No category property" $
          decodeThrow "in:\n\
                      \  time: 2017-11-23 17:42:37\n\
                      \  tags:\n  - bar\n  - baz\n"
            `shouldBe` Just cInNoCat
      it "Reading simple clock-out data" $
        decodeThrow "out:\n  time: 2017-11-23 17:42:37\n"
          `shouldBe` Just cOut

      prop "Read-write roundtrip" $ \clock ->
        let yaml = encode (clock :: Clock)
        in  decodeThrow yaml `shouldBe` Just clock

  context "Clock data aggregation" $ do

    context "Day groups" $ do
      let sameDay = (== 1) . length . group . map getDay
      prop "Grouping in days" $ \clockLog ->
        all sameDay (dayGroups clockLog)

    describe "Categories" $ do
      prop "Clock categories in allCategories" $ \clocks ->
        let clockIns    = filter isIn clocks
            categories  = mapMaybe category clockIns
        in  not (null categories) ==>
            forAll (elements categories) $ \cat ->
              cat `elem` allCategories clocks
      prop "allCategories in clocks" $ \clocks ->
        let categories = allCategories clocks
        in  not (null categories) ==>
            forAll (elements categories) $ \cat ->
              cat `elem` mapMaybe category (filter isIn clocks)

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
  arbitrary = oneof
    [ In  <$> arbitrary <*> arbitrary <*> listOf arbitrary
    , Out <$> arbitrary
    ]
