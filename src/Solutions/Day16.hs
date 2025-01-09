{-# LANGUAGE TupleSections #-}
module Solutions.Day16
  ( aoc16,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Common.Geometry
import Text.Trifecta (CharParsing (anyChar), Parser, many)
import Common.GraphUtils (dijkstra)
import qualified Data.Map as M
import Linear (V2(..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Common.Debugging (traceLns)

aoc16 :: IO ()
aoc16 = do
  printSolutions 16 $ MkAoCSolution parseInput part1
  printSolutions 16 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

data Direction = East | North | South | West deriving (Enum, Eq, Ord, Show)

part1 :: Grid Char -> Maybe Integer
part1 grid = dijkstra startingNode (getNeighbours grid) (isDestination grid)
  where
    startingNode = (East, fst . head .M.toList $ M.filter (=='S') grid)
getNeighbours :: Grid Char -> (Direction, Point) -> M.Map (Direction, Point) Integer
getNeighbours grid (directionFacing, p) = M.fromList (movementMoves ++ rotationMoves)
    where
      movedPosition = movePoint directionFacing p
      movedTile = M.findWithDefault '#' movedPosition grid
      movementMoves = [((directionFacing, movedPosition), 1) | movedTile /= '#']
      rotationMoves = map ((,1000) . (,p)) $ getRotations directionFacing

isDestination :: Grid Char ->(Direction, Point) -> Bool
isDestination grid (_, p) = 'E' == M.findWithDefault '.' p grid

getRotations :: Direction -> [Direction]
getRotations North = [East, West]
getRotations South = [East, West]
getRotations _ = [North, South]

part2 :: Grid Char -> Int
part2 grid = S.size $ snd (findBestPaths bestPathValue grid M.empty 0 [startingNode])
  where 
    startingNode = (East, fst . head .M.toList $ M.filter (=='S') grid)
    bestPathValue = fromMaybe 0 $ part1 grid

findBestPaths :: Integer -> Grid Char -> M.Map (Direction, Point) Integer -> Integer -> [(Direction, Point)] -> (M.Map (Direction, Point) Integer, S.Set Point)
findBestPaths limit grid cache cost path = 
  if isDestination grid (last path) then (cache, S.fromList (map snd path)) else forks
    where 
      neighbours = filter isBestPath . M.toList . M.filter (<=limit) . M.map (+cost) $ getNeighbours grid (last path) 
      newCacheValues = M.fromList neighbours
      cacheWithNeighbouts = M.union newCacheValues cache
      forks = foldl go (cacheWithNeighbouts, S.empty) neighbours
      go :: (M.Map (Direction, Point) Integer, S.Set Point) -> ((Direction, Point), Integer) -> (M.Map (Direction, Point) Integer, S.Set Point)
      go (newCache, points) (node, c) = (updatedCache, S.union points ps)
        where 
          (updatedCache, ps) = findBestPaths limit grid newCache c (path ++ [node])
      isBestPath :: ((Direction, Point), Integer) -> Bool
      isBestPath (node, newCost) = newCost <= M.findWithDefault limit node cache 


getDirection :: Direction -> (Int, Int)
getDirection North = (0, -1)
getDirection South = (0, 1)
getDirection East = (1, 0)
getDirection West = (-1, 0)

movePoint:: Direction -> Point -> Point
movePoint directionFacing (V2 x y) = V2 (x + dx) ( y + dy)
    where (dx, dy) = getDirection directionFacing