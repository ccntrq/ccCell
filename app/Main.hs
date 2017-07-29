module Main where

import Data.List

main :: IO ()
main = print $ runGenerations (CellAutomaton startState rule30) 1000

startState = [[True]]




type CellState = Bool
type Generation = [CellState]
type Neighborhood = (CellState,CellState,CellState)
type Rule = Neighborhood -> CellState

data CellAutomaton = CellAutomaton { getState :: [Generation]
                                   , getRule :: Rule
                                   }
instance Show CellAutomaton where
 show (CellAutomaton state _) = concat $ intersperse "\n" $ map show state

-- Rules:
-- The rules determine the new state for the center cell in a tuple of three.
-- There are 8 States this 3 Tuple can be in.
-- They are given counting down binary from 111 to 000
-- A Rule hase to define the new state for each of this States.
-- there are rules from 0 to rule 255.
-- they are built according to the 8 different states


rule30 :: Rule
rule30 (True,  True,  True)  = False
rule30 (True,  True,  False) = False
rule30 (True,  False, True)  = False
rule30 (True,  False, False) = True
rule30 (False, True,  True)  = True
rule30 (False, True,  False) = True
rule30 (False, False, True)  = True
rule30 (False, False, False) = False

runGenerations automaton cnt
  | cnt > 0 = runGenerations (runAutomaton automaton) (cnt - 1)
  | otherwise = automaton

runAutomaton automaton@(CellAutomaton state rule) = 
  let generation = head state
      generation' = runGeneration rule generation
  in appendGeneration automaton generation'

appendGeneration :: CellAutomaton -> Generation -> CellAutomaton
appendGeneration automaton generation = 
  CellAutomaton (generation:(getState automaton)) (getRule automaton)

runGeneration :: Rule -> Generation -> Generation
runGeneration rule generation = map rule $ getNeighborhoods generation

getNeighborhoods :: Generation -> [Neighborhood]
getNeighborhoods generation = getNeighborhoods' $ [False,False] ++ generation ++ [False,False]
  where getNeighborhoods' generation = 
         let neighbors = take 3 generation
         in case neighbors of
           (a:b:c:[]) -> (a,b,c):(getNeighborhoods' $ drop 1 generation)
           _ -> []
