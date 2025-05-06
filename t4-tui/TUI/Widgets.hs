module TUI.Widgets where

import Completion
import Data.List
import Brick
import Brick.Widgets.Border
import Graphics.Vty

data CompletionInput a = ComplInput
  { label       :: String
  , completion  :: Completion a
  , inFocus     :: Bool
  , input       :: String
  , selected    :: Maybe Int
  }

instance Show a => Show (CompletionInput a) where
  show (ComplInput l c f i s) =
    "C [" ++ l ++ "] (" ++ i ++ ") " ++ foc ++ " " ++ sel ++ " (" ++ cs ++ ")"
    where foc = if f then "foc" else "nof"
          sel = maybe "" show s
          cs  = intercalate ", " (show <$> complItems c)

drawCompletionInput :: CompletionInput a -> Widget ()
drawCompletionInput (ComplInput l c f i s) =
  vBox
    [ padAll 1 (str $ l ++ ":")
      <+> border (withAttr (if f then aFocused else aNormal)
            (padLeftRight 1 $ str i))
    ]

  where

        comAttrMap = attrMap defAttr
          [ (aFocused,  fg red    `withStyle` bold)
          , (aSelected, fg green  `withStyle` bold)
          , (aNormal,   defAttr)
          ]

aFocused, aSelected, aNormal :: AttrName
aFocused  = attrName "focused"
aSelected = attrName "selected"
aNormal   = attrName "normal"
