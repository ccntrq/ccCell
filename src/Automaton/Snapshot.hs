module Automaton.Snapshot
  (takeSnapshot, writeSnapshot) where

import Codec.Picture

import Automaton

-- Convenience Aliases

type CellState = Bool
type Generation = [CellState]

-- Constants/Configuration

pxWhite = PixelRGB8 (fromIntegral 255) (fromIntegral 255) (fromIntegral 255)
pxBlack = PixelRGB8 (fromIntegral 0) (fromIntegral 0) (fromIntegral 0)

pxRed   = PixelRGB8 (fromIntegral 255) (fromIntegral 0) (fromIntegral 0)
pxGreen = PixelRGB8 (fromIntegral 0) (fromIntegral 255) (fromIntegral 0)
pxBlue  = PixelRGB8 (fromIntegral 0) (fromIntegral 0) (fromIntegral 255)

defaultCellState = False

cellStateToPixel cell
  | cell = pxWhite
  | not cell = pxBlack

-- Snapshotting

writeSnapshot :: String -> Automaton -> IO()
writeSnapshot path = (writePng path) . takeSnapshot


takeSnapshot :: Automaton -> Image PixelRGB8
takeSnapshot automaton = generateImage (mkPixelFun state) width heigth
   where width = length (head state)
         heigth = length state
         state = getState automaton


mkPixelFun :: [Generation] -> (Int -> Int -> PixelRGB8)
mkPixelFun state x y = cellStateToPixel $ getCellState state x y

getCellState state x y
  | y >= heigth = defaultCellState
  | otherwise =
    let cur = state !! realY
        realX = x - ((width - (length cur)) `div` 2)
    in if realX >= 0 && realX < length cur
      then cur !! realX
      else defaultCellState
  where width = length $ head state
        heigth = length state
        realY = heigth - y - 1 -- to not have an upside down pyramide
