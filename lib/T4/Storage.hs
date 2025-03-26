module T4.Storage where

import T4.Data
import Data.List
import Data.Yaml
import System.FilePath
import System.Directory
import Control.Monad.Extra

loadDataFromDir :: FilePath -> IO [Clock]
loadDataFromDir dir = do
  ymlFiles <- filter (".yml" `isSuffixOf`) <$> listDirectory dir
  sort <$> concatMapM decodeFileThrow ((dir </>) <$> ymlFiles)
