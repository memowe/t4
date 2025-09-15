module Main where

import Completion
import T4.Data
import T4.Storage
import Util
import TUI.Widgets

import Data.Time
import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.List
import Graphics.Vty
import Lens.Micro.Platform
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import System.Process

data Tick = Tick
            deriving Show

data TUIState
  = T4StatusIn
  | T4StatusOut
  | T4ClockInCategory (List)

data T4State = T4State
  { _dir      :: FilePath
  , _clocks   :: [Clock]
  , _now      :: SimpleLocalTime
  , _durConf  :: DurationConfig
  , _compl    :: CompletionInput String
  } deriving Show
makeLenses ''T4State

main :: IO ()
main = do

  -- Prepare initial state
  dd    <- getStorageDirectory
  curr  <- getCurrentSLT
  let fooCompl    = Compl (words "foo bar baz") id
      fooComplInp = ComplInput "fO0" fooCompl True "ba" Nothing
      initState   = T4State dd [] curr manDurationConfig fooComplInp

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
  , appAttrMap      = const am
  }

inv :: AttrName
inv = attrName "inv"

am :: AttrMap
am = attrMap defAttr
  [ (inv, defAttr `withStyle` reverseVideo)
  ]

initT4 :: EventM () T4State ()
initT4 = do
  loadedClocks <- liftIO . loadDataFromDir =<< use dir
  clocks .= loadedClocks

drawT4 :: T4State -> [Widget ()]
drawT4 state = [ui]

  where ui = border $ padLeftRight 2 $ vBox
              [ header
              , hBorder
              , withAttr inv $ drawCompletionInput (state^.compl)
              ]

        header      = vLimit 1 $ hBox
                        [ lastClock (state^.clocks^?_last)
                        , fill ' '
                        , duration (state^.durConf) durPair
                        ]
        lastClock   = str . maybe "no data" summary
        duration dc = str . maybe "Not clocked in" (showDiffTime dc)
        durPair     = do  c1 <- state^.clocks^?_last
                          guard $ isIn c1
                          let t1 = getLocalTime $ time c1
                              t2 = getLocalTime $ state^.now
                          return $ diffLocalTime t2 t1

handleEvent :: BrickEvent () Tick -> EventM () T4State ()
handleEvent (AppEvent Tick) = do
  c <- liftIO getCurrentSLT
  now .= c
handleEvent (VtyEvent (EvKey KEsc []))  = halt
handleEvent e                           = return () -- liftIO $ print e
