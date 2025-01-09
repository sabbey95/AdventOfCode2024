{-# LANGUAGE TupleSections #-}
module Solutions.Day23
  ( aoc23
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, some, TokenParsing (token), letter, CharParsing (char))
import Data.List (isPrefixOf, sort, intercalate, maximumBy)
import qualified Data.Set as S
import Common.Debugging (traceLns)
import Algorithm.Search (dijkstra, bfs, dfs)
import Common.Geometry (neighbours)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import qualified Data.Map as M

aoc23 :: IO ()
aoc23 = do
  printTestSolutions 23 $ MkAoCSolution parseInput part1
  printSolutions 23 $ MkAoCSolution parseInput part2

parseInput :: Parser [(String, String)]
parseInput = some $ token parseConnection

parseConnection :: Parser (String, String)
parseConnection = do
  a <- some letter
  char '-'
  (a,) <$> some letter


part1 :: [(String, String)] -> Int
part1 = length . filter (any ("t" `isPrefixOf`)) . allThrees

allThrees :: [(String, String)] -> [[String]]
allThrees connections = S.toList . S.unions $ map getThrees connections
  where
  getConnections a = S.fromList (concatMap (\(x,y) -> if x == a then [y] else ([x | y == a])) connections)
  getThrees :: (String, String) -> S.Set[String]
  getThrees (a, b) = S.map (\s -> sort [a,b,s]) $ S.intersection (getConnections a) (getConnections b)



part2 :: [(String, String)] -> String
part2 = intercalate "," . sort . S.toList . longestConnections


longestConnections :: [(String, String)] -> S.Set String
longestConnections connections = maximumBy (compare `on` length) (concatMap go connections)
  where
  allNodes =S.toList . S.fromList $ concatMap (\(a,b) -> [a,b]) connections
  allNodesToConnections = M.fromList $ map (\a -> (a, getTheConnections a)) allNodes
  getTheConnections a = S.fromList (concatMap (\(x,y) -> if x == a then [y] else ([x | y == a])) connections)
  go (a,b) = 
    getConnections $ S.fromList [a,b]
  getConnections :: S.Set String -> [S.Set String]
  getConnections points
    | null newConnections = [points]
    | otherwise = getConnections (S.insert (head newConnections) points)
    where
      newConnections = filter (\n -> S.intersection points (M.findWithDefault S.empty n allNodesToConnections) == points) allNodes
