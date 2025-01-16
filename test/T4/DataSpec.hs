module T4.DataSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import T4.Data
import Data.Yaml
import Data.Time
import Data.Fixed

spec :: Spec
spec = do

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
