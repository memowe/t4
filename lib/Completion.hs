module Completion where

import Data.Char
import Data.List
import Data.Function
import Data.Set (Set)
import qualified Data.Set as S
import qualified System.Console.Haskeline.Completion as HC

data Completion a = Compl
  { complItems    :: Set a
  , complToString :: a -> String
  }

complMatch :: String -> String -> Bool
complMatch = isSubsequenceOf `on` map toLower

complete :: Completion a -> String -> Set a
complete (Compl xs toStr) cs = S.filter (complMatch cs . toStr) xs

haskelineCompletions :: Completion a -> String -> [HC.Completion]
haskelineCompletions c@(Compl _ toString) =
  map (HC.simpleCompletion . toString) . S.toList . complete c

haskelineCompletionFunc :: Monad m => Completion a -> HC.CompletionFunc m
haskelineCompletionFunc =
  HC.completeWord Nothing " " . (return .) . haskelineCompletions
