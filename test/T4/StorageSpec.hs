module T4.StorageSpec where

import Test.Hspec

import T4.Data
import T4.Storage
import Data.Yaml
import System.FilePath
import System.IO.Temp

spec :: Spec
spec = do
  
  context "Loading data" $ do
    let c1 = In   (simpleLocalTime 2001 2 3 4 5 6) "cat1" ["t1", "t2"]
        c2 = Out  (simpleLocalTime 2001 2 3 4 5 7)
        c3 = In   (simpleLocalTime 2001 2 4 4 5 6) "cat2" ["t2", "t3"]
    loaded <- runIO $ withSystemTempDirectory "t4" $ \tdir -> do
      encodeFile (tdir </> "2001-02-03" <.> "yml") [c2, c1]
      encodeFile (tdir </> "2001-02-04" <.> "yml") [c3]
      loadDataFromDir tdir
    it "Loaded correct (sorted) clock data" $ loaded `shouldBe` [c1, c2, c3]
