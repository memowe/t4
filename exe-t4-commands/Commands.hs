module Main where

import T4.Data
import T4.Storage
import T4.Report
import qualified Util as U
import qualified Data.Set as S
import Data.Map
import Data.Time
import Options.Applicative

data Command  = CmdIn { ccat  :: Maybe Category
                      , ctags :: [Tag]
                      }
              | CmdOut
              | CmdStatus
              | CmdCats
              | CmdTags
              | CmdReport { crepTags  :: Bool
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
          CmdReport <$> switch  (   long "tags"
                                <>  short 't'
                                <>  help "Show tags instead of categories"
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
                          addClock $ In cslt c (S.fromList ts)
handle CmdOut       = do  cslt <- U.getCurrentSLT
                          addClock $ Out cslt
handle CmdStatus    = do  clocks <- getClocks
                          putStrLn $
                            if S.null clocks
                              then "No clock data yet"
                              else summary (S.findMax clocks)
handle CmdCats      = do  clocks <- getClocks
                          mapM_ putStrLn $ allCategories clocks
handle CmdTags      = do  clocks <- getClocks
                          mapM_ putStrLn $ allTags clocks
handle (CmdReport t obl man secs) = do
  clocks <- getClocks
  let durMap = (if t then tagDurations else categoryDurations) clocks
  printDurMap obl man secs durMap

printDurMap :: Bool -> Bool -> Bool -> Map String NominalDiffTime -> IO ()
printDurMap o n s = mapM_ putStrLn . showDurMap o n s

getClocks :: IO Clocks
getClocks = loadDataFromDir =<< getStorageDirectory

main :: IO ()
main = do
  cmd <- execParser opts
  handle cmd
