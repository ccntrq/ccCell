module Main where

import Data.List

main :: IO ()
main = do
  print $ runGenerations (CellAutomaton startState (genRule 30)) 10
  print $ runGenerations (CellAutomaton startState (genRule  1)) 10

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

genRule :: Int -> Rule
genRule x
 | x < 0 || x > 255 = error $ "valid rules are from 0 to 255"
 | otherwise = rule
 where rule neighborhood =
         let results = decToBools x
         in case neighborhood of
             (True,  True,  True) -> results !! 0
             (True,  True,  False)-> results !! 1
             (True,  False, True) -> results !! 2
             (True,  False, False)-> results !! 3
             (False, True,  True) -> results !! 4
             (False, True,  False)-> results !! 5
             (False, False, True) -> results !! 6
             (False, False, False)-> results !! 7

decToBools :: Int -> [Bool]
decToBools x =
  let res = decToBools' [] x
  in pad res
  where decToBools' acc 0 = acc
        decToBools' acc x =  decToBools' (((x `mod` 2) == 1):acc) (x `div` 2)
        pad xs
          | length xs == 8 = xs
          | otherwise = pad (False:xs)


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
