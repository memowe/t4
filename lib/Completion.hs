module Completion where

import Data.Char
import Data.List
import Data.Function

data Completion a = Compl
  { complItems    :: [a]
  , complToString :: a -> String
  }

complMatch :: String -> String -> Bool
complMatch "" = const False
complMatch cs = (isSubsequenceOf `on` map toLower) cs

complete :: Completion a -> String -> [a]
complete (Compl xs toStr) cs = filter (complMatch cs . toStr) xs
