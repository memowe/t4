module Main where

import T4.Data
import T4.Storage
import T4.Report
import qualified Util as U
import Data.List
import Data.Map
import Data.Time
import Control.Monad
import Options.Applicative

data Command  = CmdIn { ccat  :: Maybe Category
                      , ctags :: [Tag]
                      }
              | CmdOut
              | CmdStatus
              | CmdCats
              | CmdTags
              | CmdReport { crepCat   :: Bool
                          , crepTags  :: Bool
                          , ordByLen  :: Bool
                          , natDur    :: Bool
                          , showSecs  :: Bool
                          }

commandParser :: Parser Command
commandParser = hsubparser
  (   command "in"          (info inParser      (progDesc "Clocking in"))
  <>  command "out"         (info outParser     (progDesc "Clocking out"))
  <>  command "status"      (info statusParser  (progDesc "Show current status"))
  <>  command "categories"  (info catsParser    (progDesc "List all categories"))
  <>  command "tags"        (info tagsParser    (progDesc "List all tags"))
  <>  command "report"      (info reportParser  (progDesc "Report"))
  )

  where outParser     = pure CmdOut
        statusParser  = pure CmdStatus
        catsParser    = pure CmdCats
        tagsParser    = pure CmdTags
        inParser      =
          CmdIn <$> optional
                      ( strOption (   long    "category"
                                  <>  short   'c'
                                  <>  metavar "CATEGORY"
                                  <>  help    "Simple name of a category"
                                  )
                      )
                <*> many
                      ( argument str
                                  (   metavar "TAGS"
                                  <>  help "List of tags, separated by space"
                                  )
                      )
        reportParser  =
          correct <$> switch  (   long "categories"
                              <>  short 'c'
                              <>  help "Include categories in the report"
                              )
                  <*> switch  (   long "tags"
                              <>  short 't'
                              <>  help "Include tags in the report"
                              )
                  <*> switch  (   long "order-by-length"
                              <>  short 'l'
                              <>  help "Reports should be ordered by length"
                              )
                  <*> switch  (   long "natural-time"
                              <>  short 'n'
                              <>  help "Natural durations instead of man-days"
                              )
                  <*> switch  (   long "show-seconds"
                              <>  short 's'
                              <>  help "Show seconds"
                              )
          where correct False False = CmdReport True  True
                correct c     t     = CmdReport c     t

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  (   fullDesc
  <>  progDesc  "Simple interface for clocking in and out"
  <>  header    "t4 - terminal time tracking tool"
  )

addClock :: Clock -> IO ()
addClock clock = do
  dd <- getStorageDirectory
  addClockToDir dd clock

handle :: Command -> IO ()
handle (CmdIn c ts) = do  cslt <- U.getCurrentSLT
                          addClock $ In cslt c ts
handle CmdOut       = do  cslt <- U.getCurrentSLT
                          addClock $ Out cslt
handle CmdStatus    = do  clocks <- getClocks
                          putStrLn $ case clocks of
                            [] -> "No clock data yet"
                            cs -> summary $ last cs
handle CmdCats      = do  clocks <- getClocks
                          mapM_ putStrLn (sort $ allCategories clocks)
handle CmdTags      = do  clocks <- getClocks
                          mapM_ putStrLn (sort $ allTags clocks)
handle (CmdReport True True obl man secs) = do
  clocks <- getClocks
  putStrLn "Categories"
  printDurMap 2 obl man secs $ categoryDurations clocks
  putStrLn "Tags"
  printDurMap 2 obl man secs $ tagDurations clocks
handle (CmdReport c t obl man secs) = do
  clocks <- getClocks
  when c $ printDurMap 0 obl man secs $ categoryDurations clocks
  when t $ printDurMap 0 obl man secs $ tagDurations clocks

printDurMap :: Int -> Bool -> Bool -> Bool -> Map String NominalDiffTime -> IO ()
printDurMap i o n s = mapM_ putStrLn . showDurMap i o n s

getClocks :: IO [Clock]
getClocks = loadDataFromDir =<< getStorageDirectory

main :: IO ()
main = do
  cmd <- execParser opts
  handle cmd
