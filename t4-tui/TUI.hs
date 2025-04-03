module Main where

import Brick
import Graphics.Vty
import T4.Data
import T4.Storage
import Lens.Micro.Platform
import Control.Monad
import Control.Monad.IO.Class

data T4State = T4State
  { _dir    :: FilePath
  , _clocks :: [Clock]
  } deriving Show
makeLenses ''T4State

main :: IO ()
main = do
  dd <- getStorageDirectory
  let initState = T4State dd []
  void $ defaultMain t4App initState

t4App :: App T4State () ()
t4App = App
  { appDraw         = drawT4
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = resizeOrQuit
  , appStartEvent   = initT4
  , appAttrMap      = const $ attrMap defAttr []
  }

initT4 :: EventM () T4State ()
initT4 = do
  loadedClocks <- liftIO . loadDataFromDir =<< use dir
  clocks .= loadedClocks

drawT4 :: T4State -> [Widget ()]
drawT4 state = lastClock ++ widgets
  where lastClock = maybe [] (return . str . summary) (state ^. clocks ^? _last)
        widgets   = [str "TODO"] -- TODO
