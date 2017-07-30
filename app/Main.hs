{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.Exit

import Automaton
import Automaton.Snapshot

data Options = Options { generations :: Int
                       , rule :: Maybe Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { generations = 100
                               &= typ "N"
                               &= help "Run for 'N' generations [100]"
                  , rule = Nothing
                        &= help "Which rule to run. [all]"
                  }
       &= summary "ccCell v0.0.3 - a Haskell cell automaton playground"
       &= program "ccCell-exe"

main :: IO ()
main = do
       opts <- cmdArgs options
       dispatch opts
       exitWith ExitSuccess

dispatch opts =
  case rule opts of
    Nothing -> writeAllRules $ generations opts
    Just x -> writeRule (generations opts) x


writeAllRules generations = do
  mapM_ (\x -> do putStrLn $ "generating rule: " ++ (show x)
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

-- TODO:
--
-- * chdir $FindBin::Bin
-- * make output dir if it does not exist
