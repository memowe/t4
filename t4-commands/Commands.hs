{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import T4.Data
import T4.Storage
import Data.Maybe
import Data.Time
import Options.Applicative
import Control.Monad.Reader
import System.Environment

data Config   = Config  { dataDir :: FilePath
                        }

data Command  = CmdIn { ccat  :: Maybe Category
                      , ctags :: [Tag]
                      }
              | CmdOut
              | CmdStatus
              deriving Show

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

configure :: IO Config
configure = do
  dd <- fromMaybe "t4-data" <$> lookupEnv "T4_DIR"
  return $ Config dd

getCurrentSLT :: IO SimpleLocalTime
getCurrentSLT = SLT . zonedTimeToLocalTime <$> getZonedTime

type T4M = ReaderT Config IO

addClock :: Clock -> T4M ()
addClock clock = do
  dd      <- asks dataDir
  clocks  <- liftIO $ loadDataFromDir dd
  liftIO $ writeDataToDir dd (clock : clocks)

handle :: Command -> T4M ()
handle (CmdIn c ts) = do  cslt <- liftIO getCurrentSLT
                          addClock $ In cslt c ts
handle CmdOut       = do  cslt <- liftIO getCurrentSLT
                          addClock $ Out cslt
handle CmdStatus    = do  clocks <- liftIO . loadDataFromDir =<< asks dataDir
                          liftIO . putStrLn $ case clocks of
                            [] -> "No clock data yet"
                            cs -> summary $ last cs

main :: IO ()
main = do
  cmd   <- execParser opts
  conf  <- configure
  runReaderT (handle cmd) conf
