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
import System.Environment
import GHC.IO.Exception

spec :: Spec
spec = do

  let c1 = In   (simpleLocalTime 2001 2 3 4 5 6) (Just "cat1") ["t1", "t2"]
      c2 = Out  (simpleLocalTime 2001 2 3 4 5 7)
      c3 = In   (simpleLocalTime 2001 2 4 4 5 6) (Just "cat2") ["t2", "t3"]

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

  context "Storage directory on disk" $ do

    describe "Storage directory name" $ do
      it "Expect ~/.t4-data" $ do
        hd <- getHomeDirectory
        sd <- getStorageDirectoryPath
        sd `shouldBe` hd </> ".t4-data"
      it "Override" $ do
        (td, sd) <- withSystemTempDirectory "override-t4" $ \tdir -> do
          (tdir,) <$> withEnv "T4DIR" tdir getStorageDirectoryPath
        sd `shouldBe` td

    describe "Storage directory predicates" $ do
      it "Exists" $ do
        isSD <- withSystemTempDirectory "empty" $ \tdir -> do
          isStorageDirectory $ tdir </> "does-not-exist"
        isSD `shouldBe` False
      it "Directory" $ do
        isSD <- withSystemTempFile "file" $ \tfile _ -> do
          isStorageDirectory tfile
        isSD `shouldBe` False
      it "Readable" $ do
        isSD <- withSystemTempDirectory "not-readable" $ \tdir -> do
          perms <- getPermissions tdir
          setPermissions tdir perms {readable = False}
          isStorageDirectory tdir
        isSD `shouldBe` False
      it "Writable" $ do
        isSD <- withSystemTempDirectory "not-writable" $ \tdir -> do
          perms <- getPermissions tdir
          setPermissions tdir perms {writable = False}
          isStorageDirectory tdir
        isSD `shouldBe` False
      it "Not only T4 yaml" $ do
        isSD <- withSystemTempDirectory "not-empty" $ \tdir -> do
          writeFile (tdir </> "foo.txt") "This is not T4 data"
          isStorageDirectory tdir
        isSD `shouldBe` False
      prop "T4 data -> OK" $ \clocks -> ioProperty $ do
        isSD <- withSystemTempDirectory "ok-t4-data" $ \tdir -> do
          writeDataToDir tdir clocks
          isStorageDirectory tdir
        isSD `shouldBe` True

    describe "Actual storage directory" $ do

      it "Storage directory gets created" $ do
        checks <- withSystemTempDirectory "container" $ \tdir -> do
          let sdName = tdir </> "t4-data"
          exists1 <- doesDirectoryExist sdName
          sd      <- withEnv "T4DIR" sdName getStorageDirectory
          exists2 <- doesDirectoryExist sdName
          isSD    <- isStorageDirectory sdName
          return (exists1, exists2, sd == sdName, isSD)
        checks `shouldBe` (False, True, True, True)

      after_ (unsetEnv "T4DIR") $ -- because withEnv gets interrupted
        it "Not a storage directory -> die" $ do
          let action = withSystemTempDirectory "dirty" $ \tdir -> do
                        writeFile (tdir </> "foo.txt") "This is not T4 data"
                        withEnv "T4DIR" tdir getStorageDirectory
          action `shouldThrow` \e ->
            "Not a t4 storage directory" `isPrefixOf` ioe_description e

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

withEnv :: String -> String -> IO a -> IO a
withEnv key value action = do
  backup <- lookupEnv key
  setEnv key value
  result <- action
  case backup of
    Just val  -> setEnv key val
    Nothing   -> unsetEnv key
  return result
