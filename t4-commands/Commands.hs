module Main where

import T4.Data
import T4.Storage
import Data.List
import Data.Time
import Data.Map (Map, (!))
import qualified Data.Map as M
import Control.Monad
import Options.Applicative
import T4.Report (categoryDurations, tagDurations)

data Command  = CmdIn { ccat  :: Maybe Category
                      , ctags :: [Tag]
                      }
              | CmdOut
              | CmdStatus
              | CmdCats
              | CmdTags
              | CmdReport { crepCat   :: Bool
                          , crepTags  :: Bool
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
                <*> some
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
          where correct False False = CmdReport True  True
                correct c     t     = CmdReport c     t

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  (   fullDesc
  <>  progDesc  "Simple interface for clocking in and out"
  <>  header    "t4 - terminal time tracking tool"
  )

getCurrentSLT :: IO SimpleLocalTime
getCurrentSLT = SLT . zonedTimeToLocalTime <$> getZonedTime

addClock :: Clock -> IO ()
addClock clock = do
  dd      <- getStorageDirectory
  clocks  <- loadDataFromDir dd
  writeDataToDir dd (clock : clocks)

handle :: Command -> IO ()
handle (CmdIn c ts)           = do  cslt <- getCurrentSLT
                                    addClock $ In cslt c ts
handle CmdOut                 = do  cslt <- getCurrentSLT
                                    addClock $ Out cslt
handle CmdStatus              = do  clocks <- getClocks
                                    putStrLn $ case clocks of
                                      [] -> "No clock data yet"
                                      cs -> summary $ last cs
handle CmdCats                = do  clocks <- getClocks
                                    mapM_ putStrLn (sort $ allCategories clocks)
handle CmdTags                = do  clocks <- getClocks
                                    mapM_ putStrLn (sort $ allTags clocks)
handle (CmdReport True  True) = do  clocks <- getClocks
                                    putStrLn "Categories"
                                    showMap 2 (categoryDurations clocks)
                                    putStrLn "Tags"
                                    showMap 2 (tagDurations clocks)
handle (CmdReport c     t)    = do  clocks <- getClocks
                                    when c $ do
                                      showMap 0 $ categoryDurations clocks
                                    when t $ do
                                      showMap 0 $ tagDurations clocks

getClocks :: IO [Clock]
getClocks = loadDataFromDir =<< getStorageDirectory

showMap :: Show a => Int -> Map String a -> IO ()
showMap indent m = do
  forM_ (M.keys m) $ \k -> do
    putStr    $ replicate indent ' '
    putStrLn  $ k ++ ": " ++ show (m ! k)

main :: IO ()
main = do
  cmd <- execParser opts
  handle cmd
