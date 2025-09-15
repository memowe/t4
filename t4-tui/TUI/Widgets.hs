module TUI.Widgets where

import Completion
import Data.List
import Brick
import Brick.Widgets.Border

data CompletionInput a = ComplInput
  { label       :: String
  , completion  :: Completion a
  , input       :: String
  , selected    :: Maybe a
  }

instance Show a => Show (CompletionInput a) where
  show (ComplInput l c f i s) =
    "C [" ++ l ++ "] (" ++ i ++ ") " ++ foc ++ " " ++ sel ++ " (" ++ cs ++ ")"
    where foc = if f then "foc" else "nof"
          sel = maybe "" show s
          cs  = intercalate ", " (show <$> complItems c)

-- drawCompletionInput :: CompletionInput a -> Widget ()
-- drawCompletionInput (ComplInput l c f i s) =
--   vBox
--     [ padAll 1 (str $ l ++ ":")
--       <+> border (padLeftRight 1 $ str i)
--     ]
