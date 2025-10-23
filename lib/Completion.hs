module Completion where

import Data.Char
import Data.List
import Data.Function
import qualified System.Console.Haskeline.Completion as HC

data Completion a = Compl
  { complItems    :: [a]
  , complToString :: a -> String
  }

complMatch :: String -> String -> Bool
complMatch = isSubsequenceOf `on` map toLower

complete :: Completion a -> String -> [a]
complete (Compl xs toStr) cs = filter (complMatch cs . toStr) xs

haskelineCompletions :: Completion a -> String -> [HC.Completion]
haskelineCompletions c@(Compl _ toString) =
  map (HC.simpleCompletion . toString) . complete c

haskelineCompletionFunc :: Monad m => Completion a -> HC.CompletionFunc m
haskelineCompletionFunc compl (x, _) =
  return ("", haskelineCompletions compl (reverse x))
