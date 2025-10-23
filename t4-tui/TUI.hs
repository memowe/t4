import Completion
import qualified System.Console.Haskeline as H

main :: IO ()
main = H.runInputTBehavior H.preferTerm (H.defaultSettings :: H.Settings IO) { H.complete = haskelineCompletionFunc fooc } $ do
  minput <- H.getInputLine "% "
  case minput of
    Nothing     -> return ()
    Just input  -> H.outputStrLn $ "Input was: " ++ input
  where fooc = Compl ["foo", "bar", "baz"] id
