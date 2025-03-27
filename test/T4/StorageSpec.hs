module T4.StorageSpec where

import Test.Hspec

import T4.Data
import T4.Storage
import Data.Yaml
import System.FilePath
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
