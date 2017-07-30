module Automaton
  ( Automaton(..)
  , simpleStartState
  , emptyStartState
  , mkRule
  , runAutomaton
  , runGenerations
  ) where

-- for 'intersperse'
import Data.List

-- Convenience Aliases

type CellState = Bool
type Generation = [CellState]
type Neighborhood = (CellState,CellState,CellState)
type Rule = Neighborhood -> CellState

-- Automaton Representation

data Automaton = Automaton { getState :: [Generation]
                           , getRule :: Rule
                           }

instance Show Automaton where
 show (Automaton state _) = concat $ intersperse "\n" $ map show state


-- Constant Start State Functions

simpleStartState = [[True]]
emptyStartState  = [[False]]


-- Rule Generation

-- A can be in 2 possible state. Either it is True or False
-- the state of each cell in the next generation is determined by the state of
-- the cell itself and its two neighbors in the current generation.
-- There are 8 states this neighborhood can be in. A rule has to determine the
-- new state for the center cell for each of this neighboorhods.
-- This makes for 256 possible rules. This mkRule function abides by a naming
-- convention for these rules called 'Wolfram code' intoduced by Stepehen
-- Wolfram
mkRule :: Int -> Rule
mkRule x
 | x < 0 || x > 255 = error $ "valid rules are from 0 to 255"
 | otherwise = rule
 where rule neighborhood =
         let newStates = numToNewStates x
         in case neighborhood of
             (True,  True,  True) -> newStates !! 0
             (True,  True,  False)-> newStates !! 1
             (True,  False, True) -> newStates !! 2
             (True,  False, False)-> newStates !! 3
             (False, True,  True) -> newStates !! 4
             (False, True,  False)-> newStates !! 5
             (False, False, True) -> newStates !! 6
             (False, False, False)-> newStates !! 7

-- convert a number from 0 to 255 to the 8 new center cell states according to
-- Wolfram Code
numToNewStates:: Int -> [Bool]
numToNewStates x =
  let res = decToBools' [] x
  in pad res
  where decToBools' acc 0 = acc
        decToBools' acc x =  decToBools' (((x `mod` 2) == 1):acc) (x `div` 2)
        pad xs
          | length xs == 8 = xs
          | otherwise = pad (False:xs)


-- Running the automaton

-- run the automaton for 'cnt' generations
runGenerations automaton cnt
  | cnt > 0 = runGenerations (runAutomaton automaton) (cnt - 1)
  | otherwise = automaton

runAutomaton automaton@(Automaton state rule) =
  let generation = head state
      generation' = runGeneration rule generation
  in appendGeneration automaton generation'

appendGeneration :: Automaton -> Generation -> Automaton
appendGeneration automaton generation =
  Automaton (generation:(getState automaton)) (getRule automaton)

runGeneration :: Rule -> Generation -> Generation
runGeneration rule generation = map rule $ getNeighborhoods generation

getNeighborhoods :: Generation -> [Neighborhood]
getNeighborhoods generation =
  getNeighborhoods' $ [False,False] ++ generation ++ [False,False]
  where getNeighborhoods' generation =
         let neighbors = take 3 generation
         in case neighbors of
           (a:b:c:[]) -> (a,b,c):(getNeighborhoods' $ drop 1 generation)
           _ -> []


