module T4.DataSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import T4.Data
import Data.Char
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
    describe "JSON" $ do
      it "Correct simple JSONification" $
        toJSON theTime `shouldBe` String iso8601Time
      it "Correct simple parsing" $
        A.fromJSON (String iso8601Time) `shouldBe` A.Success theTime
      prop "SLT-JSON-SLT roundtrip" $ \slt ->
        let mslt = do String iso <- return $ toJSON (slt :: SimpleLocalTime)
                      readMaybe $ unpack iso
        in  mslt `shouldBe` Just slt

  context "Config data conversion" $ do
    it "Reading simple config" $
      decodeThrow "allCats:\n- foo\n- bar\nallTags:\n- baz\n- quux\n"
        `shouldBe` Just (Config ["foo", "bar"] ["baz", "quux"])
    prop "Read-Write roundtrip" $ \cfg ->
      let yaml = encode (cfg :: Config)
      in  decodeThrow yaml `shouldBe` Just cfg

  context "Clock in/out data conversion" $ do
    let theTime = SLT $ LocalTime (fromGregorian 2017 11 23)
                                  (TimeOfDay 17 42 37)
    it "Reading simple clock-in data" $
      decodeThrow "in:\n\
                  \  time: 2017-11-23 17:42:37\n\
                  \  cat: foo\n\
                  \  tags:\n  - bar\n  - baz\n"
        `shouldBe` Just (In theTime "foo" ["bar", "baz"])
    it "Reading simple clock-out data" $
      decodeThrow "out:\n  time: 2017-11-23 17:42:37\n"
        `shouldBe` Just (Out theTime)
    prop "Read-write roundtrip" $ \clock ->
      let yaml = encode (clock :: Clock)
      in  decodeThrow yaml `shouldBe` Just clock

instance Read SimpleLocalTime where
  readsPrec _ = readP_to_S $ SLT <$> do
    y <-              dig 4
    m <- char '-' >>  dig 2
    d <- char '-' >>  dig 2
    h <- char ' ' >>  dig 2
    i <- char ':' >>  dig 2
    s <- char ':' >>  dig 2
    return $ LocalTime (fromGregorian y m d) (TimeOfDay h i s)
    where dig n = read <$> count n (satisfy isDigit)

instance Arbitrary Config where
  arbitrary = Config  <$> listOf (arbitrary `suchThat` valid)
                      <*> listOf (arbitrary `suchThat` valid)
    where valid = (',' `notElem`)

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
