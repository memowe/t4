import T4.Data
import T4.Storage
import T4.Report
import Util
import Completion
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Data.Map (Map)
import Data.Time
import qualified System.Console.Haskeline as H

main :: IO ()
main = do
  sdir  <- getStorageDirectory
  clock <- lastMaybe <$> loadDataFromDir sdir
  showState clock
  newClock <- if isJust clock && isIn (fromJust clock)
                then promptIn (time $ fromJust clock)
                else promptOut
  case newClock of
    Nothing -> showState clock
    Just c  -> do addClockToDir sdir c
                  showState (Just c)

showState :: Maybe Clock -> IO ()
showState = putStrLn . maybe "No clock data yet" summary

promptIn :: SimpleLocalTime -> IO (Maybe Clock)
promptIn started = do
  showSpent started
  clocks <- loadDataFromDir =<< getStorageDirectory
  choice <- run $ H.getInputChar
    "[o]ut - [u]pdate - report [c]ategories - report [t]ags - [q]uit: "
  case choice of
    Just 'o'  -> showSpent started >> Just . Out <$> getCurrentSLT
    Just 'u'  -> Just <$> clockIn
    Just 'c'  -> Nothing <$ report "Categories" (categoryDurations clocks)
    Just 't'  -> Nothing <$ report "Tags"       (tagDurations clocks)
    _         -> return Nothing

showSpent :: SimpleLocalTime -> IO ()
showSpent started = do
  now <- getCurrentSLT
  putStrLn $ "Spent: " ++ showDuration (now `minusTime` started)
  where showDuration  = showDiffTime naturalDurationConfig
        minusTime     = diffLocalTime `on` getLocalTime

promptOut :: IO (Maybe Clock)
promptOut = do
  clocks <- loadDataFromDir =<< getStorageDirectory
  choice <- run $ H.getInputChar
    "[i]n - report [c]ategories - report [t]ags - [q]uit: "
  case choice of
    Just 'i'  -> Just <$> clockIn
    Just 'c'  -> Nothing <$ report "Categories" (categoryDurations clocks)
    Just 't'  -> Nothing <$ report "Tags"       (tagDurations clocks)
    _         -> return Nothing

clockIn :: IO Clock
clockIn = do
  clocks <- loadDataFromDir =<< getStorageDirectory
  let catsCompl = (`Compl` id) $ allCategories clocks
      tagsCompl = (`Compl` id) $ allTags clocks
  now   <- getCurrentSLT
  mc    <- runWithCompletion catsCompl $ H.getInputLine "Category: "
  mtags <- runWithCompletion tagsCompl $ H.getInputLine "Tags: "
  return $ In now (parseCat mc) (parseTags mtags)
  where parseCat  = fmap $ dropWhile isSpace . dropWhileEnd isSpace
        parseTags = map (dropWhile (== '#')) . words . fromMaybe ""

report :: String -> Map String NominalDiffTime -> IO ()
report prefix durMap = do
  putStrLn "[s]econds - [l]ength-ordered - [n]atural time instead of man-days"
  mOptions <- run $ H.getInputLine "Options: "
  case mOptions of
    Nothing       -> return ()
    Just options  -> do
      putStrLn prefix
      let optSecs = 's' `elem` options
          optLen  = 'l' `elem` options
          optNat  = 'n' `elem` options
      mapM_ putStrLn $ showDurMap optLen optNat optSecs durMap

run :: H.InputT IO a -> IO a
run = H.runInputTBehavior H.preferTerm H.defaultSettings

runWithCompletion :: Completion c -> H.InputT IO a -> IO a
runWithCompletion compl = H.runInputTBehavior H.preferTerm settings
  where settings  = (H.defaultSettings :: H.Settings IO) {H.complete = hcompl}
        hcompl    = haskelineCompletionFunc compl
