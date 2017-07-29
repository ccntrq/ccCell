module Main where

import System.Environment
import System.Exit

import Automaton
import Automaton.Snapshot

usage   = do
             putStrLn "Usage: ccCell-exe [-h] generations rule?"
             putStrLn ""
             putStrLn "       ccCell - elementary cell automaton"
             putStrLn ""
             putStrLn "       given rule:"
             putStrLn "       writes a png representation of that rule after 'generations'"
             putStrLn "       generations to './images/generations/rule'"
             putStrLn ""
             putStrLn "       without rule:"
             putStrLn "       creates an image for each rule"

main :: IO ()
main = do
       args <- getArgs
       dispatch args
       exit


dispatch ["-h"] = usage
dispatch args =
 case length args of
   1 -> writeAllRules $ read (head args)
   2 -> let generations = read (head args)
            rule = read (args !! 1)
        in writeRule generations rule
   _ -> usage

writeAllRules generations = do
  mapM (\x -> do putStrLn $ "generating rule: " ++ (show x)
                 writeRule generations x) 
       [0..255]
  return ()

writeRule generations rule =
  writeSnapshot fileName $ runGenerations (Automaton simpleStartState (mkRule rule)) generations
  where fileName = "./images/" ++  (show generations)  ++ "/rule"  ++ ruleNum ++ ".png"
        ruleNum = let str  = show rule
                  in case length str of
                   1 -> "00" ++ str
                   2 -> "0" ++ str
                   3 -> str

exit    = exitWith ExitSuccess

-- TODO:
--
-- * chdir $FindBin::Bin
-- * make output dir if it does not exist
