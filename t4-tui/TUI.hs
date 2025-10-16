module Main where

import T4.Data
import T4.Storage
import Util
import Data.Time
import Brick
import Brick.BChan
import Brick.Widgets.Border
import Graphics.Vty
import Lens.Micro.Platform
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import System.Process

data Tick = Tick
            deriving Show

data T4State = T4State
  { _dir      :: FilePath
  , _clocks   :: [Clock]
  , _now      :: SimpleLocalTime
  , _durConf  :: DurationConfig
  } deriving Show
makeLenses ''T4State

main :: IO ()
main = do

  -- Prepare initial state
  dd    <- getStorageDirectory
  curr  <- getCurrentSLT
  let initState = T4State dd [] curr manDurationConfig

  -- Prepare ticking thread
  tickChan      <- newBChan 42
  tickThreadID  <- forkIO $ forever $ do  writeBChan tickChan Tick
                                          threadDelay $ 1000 * 1000 -- 1 sec.
  -- Go
  void $ customMainWithDefaultVty (Just tickChan) t4App initState

  -- Cleanup
  callCommand "clear"
  killThread tickThreadID

t4App :: App T4State Tick ()
t4App = App
  { appDraw         = drawT4
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = initT4
  , appAttrMap      = const $ attrMap defAttr []
  }

initT4 :: EventM () T4State ()
initT4 = do
  loadedClocks <- liftIO . loadDataFromDir =<< use dir
  clocks .= loadedClocks

drawT4 :: T4State -> [Widget ()]
drawT4 state = [ui]
  where ui = hBox [ box (lastClock (state^.clocks^?_last))
                  , fill ' '
                  , box (duration (state^.durConf) durPair)
                  ]
        durPair = do  c1 <- state^.clocks^?_last
                      guard $ isIn c1
                      let t1 = getLocalTime $ time c1
                          t2 = getLocalTime $ state^.now
                      return $ diffLocalTime t2 t1
        box     = border . padLeftRight 2

lastClock :: Maybe Clock -> Widget ()
lastClock = str . maybe "no data" summary

duration :: DurationConfig -> Maybe NominalDiffTime -> Widget ()
duration dc = str . maybe "Not clocked in" (showDiffTime dc)

handleEvent :: BrickEvent () Tick -> EventM () T4State ()
handleEvent (AppEvent Tick) = do
  c <- liftIO getCurrentSLT
  now .= c
  return ()
handleEvent (VtyEvent (EvKey KEsc []))  = halt
handleEvent e                           = liftIO $ print e
