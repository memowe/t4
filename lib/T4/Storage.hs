module T4.Storage where

import T4.Data
import Data.List
import Data.Maybe
import Data.Yaml
import Text.Regex.TDFA
import System.FilePath
import System.Directory
import System.Environment
import Control.Monad.Extra

loadDataFromDir :: FilePath -> IO [Clock]
loadDataFromDir dir = do
  ymlFiles <- filter (".yml" `isSuffixOf`) <$> listDirectory dir
  sort <$> concatMapM decodeFileThrow ((dir </>) <$> ymlFiles)

writeDataToDir :: FilePath -> [Clock] -> IO ()
writeDataToDir dir clocks = do
  forM_ (dayGroups clocks) $ \dayGroup -> do
    let day = dateString (time $ head dayGroup)
    encodeFile (dir </> day <.> "yml") dayGroup

getStorageDirectoryPath :: IO FilePath
getStorageDirectoryPath = do
  hd <- getHomeDirectory
  fromMaybe (hd </> ".t4-data") <$> lookupEnv "T4DIR"

isStorageDirectory :: FilePath -> IO Bool
isStorageDirectory sd = andM [exists, readablePerm, writablePerm, onlyT4Yaml]
  where exists        = doesDirectoryExist sd
        readablePerm  = readable <$> getPermissions sd
        writablePerm  = writable <$> getPermissions sd
        onlyT4Yaml    = all (=~ regex) <$> listDirectory sd
        regex         = "^[0-9]{4}-[0-9]{2}-[0-9]{2}.yml$" :: String

getStorageDirectory :: IO FilePath
getStorageDirectory = do
  sd <- getStorageDirectoryPath
  unlessM (doesDirectoryExist sd) $
    createDirectory sd
  unlessM (isStorageDirectory sd) $
    fail ("Not a t4 storage directory: " ++ sd)
  return sd
