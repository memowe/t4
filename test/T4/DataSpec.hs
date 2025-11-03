module T4.DataSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import T4.Data
import Data.Char
import Data.Maybe
import Data.Text (unpack, pack)
import qualified Data.Set as S
import qualified Data.Map as M
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
        cIn       = In theTime (Just "foo") (S.fromList ["bar", "baz"])
        cInNoCat  = In theTime Nothing      (S.fromList ["bar", "baz"])
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
        summary cIn `shouldBe` "IN (2017-11-23 17:42:37) [foo] #bar #baz"
      it "Clock in without category" $
        summary cInNoCat `shouldBe` "IN (2017-11-23 17:42:37) #bar #baz"
      it "Clock out" $
        summary cOut `shouldBe` "OUT (2017-11-23 17:42:37)"

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
      prop "Grouping in days" $ \clockLog ->
        not (null clockLog) ==>
        forAll (elements $ M.toList $ dayGroups clockLog) $ \(d, cs) ->
          forAll (elements $ S.toList cs) $ \c ->
            getDay c `shouldBe` d

    describe "Categories" $ do
      prop "Clock categories in allCategories" $ \clocks ->
        let clockIns    = S.filter isIn clocks
            categories  = mapMaybe category (S.toList clockIns)
        in  not (null categories) ==>
            forAll (elements categories) $ \cat ->
              cat `S.member` allCategories clocks
      prop "allCategories in clocks" $ \clocks ->
        let categories = allCategories clocks
        in  not (null categories) ==>
            forAll (elements $ S.toList categories) $ \cat ->
              let clockIns  = S.filter isIn clocks
              in  cat `elem` mapMaybe category (S.toList clockIns)

    describe "Tags" $ do
      prop "Clock tags in allTags" $ \clocks ->
        let clockIns = S.filter isIn clocks
        in  not (null clockIns) ==>
            forAll (elements $ S.toList clockIns) $ \clock ->
              let cTags = tags clock
              in  not (null cTags) ==>
              forAll (elements $ S.toList cTags) $ \tag ->
                tag `S.member` allTags clocks
      prop "allTags in clocks" $ \clocks ->
        let theTags = allTags clocks
        in  not (null theTags) ==>
            forAll (elements $ S.toList theTags) $ \tag ->
              let cTags = S.unions . S.map tags . S.filter isIn $ clocks
              in  tag `S.member` cTags

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
    [ In  <$> arbitrary
          <*> arbitrary
          <*> (S.fromList <$> listOf arbitrary)
    , Out <$> arbitrary
    ]
