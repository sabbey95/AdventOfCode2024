{-# LANGUAGE TupleSections #-}

module Solutions.Day18
  ( aoc18,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging (traceLns)
import Common.Geometry (Point, allOrthogonalNeighbours, renderVectorSet)
import Common.GraphUtils (dijkstra)
import Control.Lens ((^.))
import Data.IntMap (difference)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Linear (R1 (_x), R2 (_y), V2 (..), ex)
import Text.Trifecta (Parser, comma, integer', some, token)
import qualified Data.Foldable as S

aoc18 :: IO ()
aoc18 = do
  printSolutions 18 $ MkAoCSolution parseInput part1
  printSolutions 18 $ MkAoCSolution parseInput part2

parseInput :: Parser [Point]
parseInput = some $ token parseCoord

parseCoord :: Parser Point
parseCoord = do
  x <- integer'
  comma
  V2 (fromInteger x) . fromInteger <$> integer'

part1 :: [Point] -> Maybe Integer
part1 = solve . S.fromList . take 1024

solve :: S.Set Point -> Maybe Integer
solve points = dijkstra startingNode getNeighbours (== endingNode)
  where
    startingNode = V2 0 0
    endingNode = V2 70 70
    getNeighbours p = M.fromSet (const 1) . S.filter (\(V2 px py) -> px >= startingNode ^. _x && px <= endingNode ^. _x && py >= startingNode ^. _y && py <= endingNode ^. _y) $ S.difference (allOrthogonalNeighbours p) points

part2 :: [Point] -> Point
part2 = solvePt2 2500

solvePt2 :: Int -> [Point] -> Point
solvePt2 n points = case path of 
    Nothing -> last nextPoints
    Just _ -> solvePt2 (n+1) points
  where 
    nextPoints = take n points
    path = solve $ S.fromList nextPoints
