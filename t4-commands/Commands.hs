module Main where

import T4.Data
import T4.Storage
import Data.Time
import Options.Applicative

data Command  = CmdIn { ccat  :: Maybe Category
                      , ctags :: [Tag]
                      }
              | CmdOut
              | CmdStatus

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

commandParser :: Parser Command
commandParser = hsubparser
  (   command "in"      (info inParser      (progDesc "Clocking in"))
  <>  command "out"     (info outParser     (progDesc "Clocking out"))
  <>  command "status"  (info statusParser  (progDesc "Show current status"))
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
handle (CmdIn c ts) = do  cslt <- getCurrentSLT
                          addClock $ In cslt c ts
handle CmdOut       = do  cslt <- getCurrentSLT
                          addClock $ Out cslt
handle CmdStatus    = do  dd      <- getStorageDirectory
                          clocks  <- loadDataFromDir dd
                          putStrLn $ case clocks of
                            [] -> "No clock data yet"
                            cs -> summary $ last cs

main :: IO ()
main = do
  cmd <- execParser opts
  handle cmd
