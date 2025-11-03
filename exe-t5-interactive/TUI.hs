import T4.Data
import T4.Storage
import T4.Report
import Util
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import Data.Time
import qualified System.Console.Haskeline as H
import qualified System.Console.Haskeline.Completion as HC

main :: IO ()
main = do
  sdir  <- getStorageDirectory
  clock <- findMax <$> loadDataFromDir sdir
  showState clock
  newClock <- if isJust clock && isIn (fromJust clock)
                then promptIn (time $ fromJust clock)
                else promptOut
  case newClock of
    Nothing -> showState clock
    Just c  -> do addClockToDir sdir c
                  showState (Just c)
  where findMax = fmap fst . S.maxView

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
  now     <- getCurrentSLT
  clocks  <- loadDataFromDir =<< getStorageDirectory
  mcat    <- inputCompl (allCategories clocks)  "Category: "
  mtags   <- inputCompl (allTags clocks)        "Tags: "
  return $ In now (parseCat mcat) (parseTags mtags)
  where parseCat  = fmap $ dropWhile isSpace . dropWhileEnd isSpace
        parseTags = S.fromList . map (dropWhile (== '#')) . words . fromMaybe ""

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

inputCompl :: Set String -> String -> IO (Maybe String)
inputCompl items = H.runInputTBehavior H.preferTerm settings . H.getInputLine
  where settings = (H.defaultSettings :: H.Settings IO) {H.complete = complete}
        complete = HC.completeWord Nothing " " (return . S.toList . compls)
        compls s = S.map HC.simpleCompletion $ S.filter (s `isPrefOf`) items
        isPrefOf = isPrefixOf `on` map toLower
