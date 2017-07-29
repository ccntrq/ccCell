module Main where

import Automaton
import Automaton.Snapshot

main :: IO ()
main = do
  print $ runGenerations (Automaton simpleStartState (mkRule 30)) 10
  print $ runGenerations (Automaton emptyStartState (mkRule 30)) 10
  print $ runGenerations (Automaton simpleStartState (mkRule 1)) 10
  print $ runGenerations (Automaton emptyStartState (mkRule 1)) 10
  writeSnapshot "./automaton.png" (runGenerations (Automaton simpleStartState (mkRule 30)) 100)
