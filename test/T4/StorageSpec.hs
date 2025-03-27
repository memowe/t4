module T4.StorageSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import T4.Data
import T4.Storage
import T4.DataSpec () -- Arbitrary Clock instance
import Data.List
import Data.Yaml
import Data.Time
import System.FilePath
import System.Directory
import System.IO.Temp

spec :: Spec
spec = do

  let c1 = In   (simpleLocalTime 2001 2 3 4 5 6) "cat1" ["t1", "t2"]
      c2 = Out  (simpleLocalTime 2001 2 3 4 5 7)
      c3 = In   (simpleLocalTime 2001 2 4 4 5 6) "cat2" ["t2", "t3"]
  
  context "Loading data" $ do
    loaded <- runIO $ withSystemTempDirectory "t4" $ \tdir -> do
      encodeFile (tdir </> "2001-02-03" <.> "yml") [c2, c1]
      encodeFile (tdir </> "2001-02-04" <.> "yml") [c3]
      loadDataFromDir tdir
    it "Loaded correct (sorted) clock data" $ loaded `shouldBe` [c1, c2, c3]

  context "Storing data" $ do
    (cs1, cs2) <- runIO $ withSystemTempDirectory "t4" $ \tdir -> do
      writeDataToDir tdir [c2, c1, c3]
      cs1 <- decodeFileThrow (tdir </> "2001-02-03" <.> "yml")
      cs2 <- decodeFileThrow (tdir </> "2001-02-04" <.> "yml")
      return (cs1, cs2)
    it "Correct clocks in first file"   $ cs1 `shouldBe` [c1, c2]
    it "Correct clocks in second file"  $ cs2 `shouldBe` [c3]

  prop "Simple roundtrip Clocks-YAML-Clocks" $ \clocks -> ioProperty $ do
    loaded <- withSystemTempDirectory "t4" $ \tdir -> do
      writeDataToDir tdir clocks
      loadDataFromDir tdir
    return $ loaded === sort clocks

  prop "Correct file name" $ \clock -> ioProperty $ do
    filenames <- withSystemTempDirectory "t4" $ \tdir -> do
      writeDataToDir tdir [clock]
      listDirectory tdir
    return $ filenames === [dateString (time clock) <.> "yml"]

  prop "Same file => same day" $ \clocks ->
    not (null clocks) ==> ioProperty $ do
      fileClocks <- withSystemTempDirectory "t4" $ \tdir -> do
        writeDataToDir tdir clocks
        listDirectory tdir >>= mapM (decodeFileThrow . (tdir </>))
      return $
        forAll (elements fileClocks) $ \rclocks ->
          allEqual (localDay . getLocalTime . time <$> rclocks)

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs
