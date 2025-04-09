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

inParser :: Parser Command
inParser =
  CmdIn <$> optional
              ( strOption (   long    "category"
                          <>  short   'c'
                          <>  metavar "CATEGORY"
                          <>  help    "Simple name of a category"
                          )
              )
        <*> some
              ( argument  str
                          (   metavar "TAGS"
                          <>  help "List of tags, separated by space"
                          )
              )

outParser :: Parser Command
outParser = pure CmdOut

statusParser :: Parser Command
statusParser = pure CmdStatus

catsParser :: Parser Command
catsParser = pure CmdCats

tagsParser :: Parser Command
tagsParser = pure CmdTags

reportParser :: Parser Command
reportParser = correct <$> catSwitch <*> tagSwitch
  where catSwitch = switch
                      (   long "categories"
                      <>  short 'c'
                      <>  help "Include categories in the report"
                      )
        tagSwitch = switch
                      (   long "tags"
                      <>  short 't'
                      <>  help "Include tags in the report"
                      )
        correct False False = CmdReport True  True
        correct c     t     = CmdReport c     t

commandParser :: Parser Command
commandParser = hsubparser
  (   command "in"          (info inParser      (progDesc "Clocking in"))
  <>  command "out"         (info outParser     (progDesc "Clocking out"))
  <>  command "status"      (info statusParser  (progDesc "Show current status"))
  <>  command "categories"  (info catsParser    (progDesc "List all categories"))
  <>  command "tags"        (info tagsParser    (progDesc "List all tags"))
  <>  command "report"      (info reportParser  (progDesc "Report"))
  )

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
handle CmdStatus              = do  dd      <- getStorageDirectory
                                    clocks  <- loadDataFromDir dd
                                    putStrLn $ case clocks of
                                      [] -> "No clock data yet"
                                      cs -> summary $ last cs
handle CmdCats                = do  dd      <- getStorageDirectory
                                    clocks  <- loadDataFromDir dd
                                    mapM_ putStrLn (sort $ allCategories clocks)
handle CmdTags                = do  dd      <- getStorageDirectory
                                    clocks  <- loadDataFromDir dd
                                    mapM_ putStrLn (sort $ allTags clocks)
handle (CmdReport True  True) = do  dd      <- getStorageDirectory
                                    clocks  <- loadDataFromDir dd
                                    putStrLn "Categories"
                                    showMap 2 (categoryDurations clocks)
                                    putStrLn "Tags"
                                    showMap 2 (tagDurations clocks)
handle (CmdReport c     t)    = do  dd      <- getStorageDirectory
                                    clocks  <- loadDataFromDir dd
                                    when c $ showMap 0 $ categoryDurations  clocks
                                    when t $ showMap 0 $ tagDurations       clocks

showMap :: Show a => Int -> Map String a -> IO ()
showMap indent m = do
  forM_ (M.keys m) $ \k -> do
    putStr    $ replicate indent ' '
    putStrLn  $ k ++ ": " ++ show (m ! k)

main :: IO ()
main = do
  cmd <- execParser opts
  handle cmd
