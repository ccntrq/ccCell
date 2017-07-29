module Automaton.Snapshot
  (takeSnapshot, writeSnapshot) where

import Codec.Picture

import Automaton

-- Convenience Aliases

type CellState = Bool
type Generation = [CellState]

writeSnapshot :: String -> Automaton -> IO()
writeSnapshot path = (writePng path) . takeSnapshot


takeSnapshot :: Automaton -> Image PixelRGB8
takeSnapshot automaton = generateImage (mkPixelFun (getState automaton)) width heigth
   where width = length (head (getState automaton))
         heigth = length (getState automaton)


mkPixelFun :: [Generation] -> (Int -> Int -> PixelRGB8)
mkPixelFun state =
  let pixelFun state x y = PixelRGB8  (fromIntegral(if ((state !! y ) !! x) then 255 else 0)) (fromIntegral(if ((state !! y ) !! x) then 255 else 0)) (fromIntegral(if ((state !! y ) !! x) then 255 else 0))
  in pixelFun (reverse filledState)
  where filledState = map fillGeneration state
        fillGeneration gen =
          let toShort = width - (length gen)
              eachSide = take (toShort `div` 2) $ repeat False
          in eachSide ++ gen ++ eachSide
        width = length $ head state
