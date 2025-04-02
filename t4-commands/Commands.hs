module Main where

import T4.Data
import Options.Applicative

data Command  = CmdIn { ccat  :: Category
                      , ctags :: [Tag]
                      }
              | CmdOut
              | CmdStatus
              deriving Show

inParser :: Parser Command
inParser =
  CmdIn <$> strOption (   long    "category"
                      <>  short   'c'
                      <>  metavar "CATEGORY"
                      <>  value   ""
                      <>  help    "Simple name of a category"
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

main :: IO ()
main = do
  print =<< execParser opts
