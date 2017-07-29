module Main where

import Automaton
import Automaton.Snapshot

main :: IO ()
main = do
  writeAllRules 100
  print "wrote sample images of all rules to ./images"

writeAllRules generations = mapM writeRule [0..255]
 where writeRule x = writeSnapshot fileName $ runGenerations (Automaton simpleStartState (mkRule x)) generations
         where fileName = "./images/" ++  (show generations)  ++ "/rule"  ++ ruleNum ++ ".png"
               ruleNum = let str  = show x
                         in case length str of
                          1 -> "00" ++ str
                          2 -> "0" ++ str
                          3 -> str
