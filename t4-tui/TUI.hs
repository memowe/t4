import T4.Data
import T4.Storage
import Util
import Completion
import Data.Maybe
import qualified System.Console.Haskeline as H

main :: IO ()
main = do
  sdir  <- getStorageDirectory
  clock <- lastMaybe <$> loadDataFromDir sdir
  showState clock
  newClock <- if isJust clock && isIn (fromJust clock)
                then promptIn
                else promptOut
  case newClock of
    Nothing -> showState clock
    Just c  -> do addClockToDir sdir c
                  showState (Just c)

showState :: Maybe Clock -> IO ()
showState = putStrLn . maybe "No clock data yet" summary

promptIn :: IO (Maybe Clock)
promptIn = do
  choice <- run $ H.getInputChar "[o]ut - [c]hange - [q]uit: "
  case choice of
    Just 'o'  -> Just . Out <$> getCurrentSLT
    Just 'c'  -> Just <$> clockIn
    _         -> return Nothing

promptOut :: IO (Maybe Clock)
promptOut = do
  choice <- run $ H.getInputChar "[i]n - [q]uit: "
  case choice of
    Just 'i'  -> Just <$> clockIn
    _         -> return Nothing

clockIn :: IO Clock
clockIn = do
  clocks <- loadDataFromDir =<< getStorageDirectory
  let catsCompl = (`Compl` id) $ allCategories clocks
      tagsCompl = (`Compl` id) $ allTags clocks
  now   <- getCurrentSLT
  mc    <- runWithCompletion catsCompl $ H.getInputLine "Category: "
  mtags <- runWithCompletion tagsCompl $ H.getInputLine "Tags: "
  return $ In now mc (parseTags mtags)
  where parseTags = map (dropWhile (== '#')) . words . fromMaybe []

run :: H.InputT IO a -> IO a
run = H.runInputTBehavior H.preferTerm H.defaultSettings

runWithCompletion :: Completion c -> H.InputT IO a -> IO a
runWithCompletion compl = H.runInputTBehavior H.preferTerm settings
  where settings  = (H.defaultSettings :: H.Settings IO) {H.complete = hcompl}
        hcompl    = haskelineCompletionFunc compl
