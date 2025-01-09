module Solutions.Day20
  ( aoc20,
  )
where

import Algorithm.Search (dfs, dijkstra)
import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging (traceLns)
import Common.Geometry (Grid, Point, allOrthogonalNeighbours, enumerateMultilineStringToVectorMap, maxPoints, neighbours, renderVectorMap)
import Control.Lens ((^.))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ
import qualified Data.Set as S
import Linear (R1 (_x), R2 (_y), V2 (..), ex)
import Text.Trifecta (Parser, anyChar, many)

aoc20 :: IO ()
aoc20 = do
  printSolutions 20 $ MkAoCSolution parseInput part1
  printSolutions 20 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

part1 :: Grid Char -> Int
part1 = solve 2

getPath :: S.Set Point -> Point -> Point -> Point -> [Point]
getPath walls start end p =
  fromMaybe [] $ dfs sofa (== end) start
  where
    sofa :: Point -> [Point]
    sofa p = if p `elem` neighbours then [p] else neighbours
      where
        neighbours = S.toList $ S.difference (allOrthogonalNeighbours p) walls

part2 :: Grid Char -> Int
part2 = solve 20

solve :: Int -> Grid Char -> Int
solve numberToCheck grid = countPoints path
  where
    (startingNode, endingNode) = maxPoints grid
    start = fst . head . M.toList $ M.filter (== 'S') grid
    end = fst . head . M.toList $ M.filter (== 'E') grid
    allWalls = M.keys $ M.filter (== '#') grid
    wallsSet = S.fromList allWalls
    walls = filter (\(V2 x y) -> x /= startingNode ^. _x && y /= startingNode ^. _y && x /= endingNode ^. _x && y /= endingNode ^. _y) allWalls
    path =
      start : getPath wallsSet start end end
    pathWithWeights = M.fromList $ zip (reverse path) [0 ..]
    pathSet = S.fromList path

    countPoints :: [Point] -> Int
    countPoints [p] = 0
    countPoints (p : ps) = length allReachable + countPoints ps
      where
        allReachable = filter (\p2 -> manhattanDistance p p2 <= numberToCheck && checkWallPoint p p2) ps

    checkWallPoint :: V2 Int -> V2 Int -> Bool
    checkWallPoint p1 p2 = M.findWithDefault 0 p1 pathWithWeights - M.findWithDefault 0 p2 pathWithWeights - manhattanDistance p1 p2 >= 100

manhattanDistance :: V2 Int -> V2 Int -> Int
manhattanDistance (V2 x y) (V2 x2 y2) = abs (x2 -x) + abs (y2 - y)