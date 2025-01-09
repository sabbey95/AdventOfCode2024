{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Solutions.Day6
  ( aoc6,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging (traceLns)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Text.Trifecta (CharParsing (anyChar), Parser, letter, some, token, try)

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

data GridPoint = Wall | Space | Guard Direction deriving (Eq, Show, Ord)

data Direction = N | S | E | W deriving (Enum, Eq, Show, Ord)

type Grid = M.Map (Int, Int) GridPoint

parseInput :: Parser Grid
parseInput = do
  gridLines <- some $ token $ some $ try parseGridPoint
  let xCoords = length (head gridLines)
  let yCoords = length gridLines
  let coords = [(x, y) | y <- [0 .. yCoords - 1], x <- [0 .. xCoords - 1]]
  pure . M.fromList $ zip coords (concat gridLines)

parseGridPoint :: Parser GridPoint
parseGridPoint = do
  c <- anyChar
  case c of
    '.' -> pure Space
    '#' -> pure Wall
    '^' -> pure $ Guard N
    '>' -> pure $ Guard E
    'v' -> pure $ Guard S
    '<' -> pure $ Guard W
    c -> fail ("Unrecognised char: " ++ [c])

part1 :: Grid -> Int
part1 x = S.size . S.map fst . fst $ walkThrough guard x S.empty
  where
    guard =
      head . mapMaybe liftTupleMaybe . M.toList $
        M.map
          ( \p -> case p of
              Guard d -> pure d
              _ -> Nothing
          )
          x
    updatedGrid = M.insert (fst guard) Space x

walkThrough :: ((Int, Int), Direction) -> Grid -> S.Set ((Int, Int), Direction) -> (S.Set ((Int, Int), Direction), Bool)
walkThrough (guardP, guardD) grid result = if (guardP, guardD) `S.member` result then (result, True) else newPath
  where
    endPoint = doStep guardP guardD
    newPath = case M.lookup endPoint grid of
      Just Wall -> walkThrough (guardP, nextDirection guardD) grid result
      Just Space -> walkThrough (endPoint, guardD) grid $ S.insert (guardP, guardD) result
      _ -> (S.insert (guardP, guardD) result, False)

doStep :: (Int, Int) -> Direction -> (Int, Int)
doStep (x, y) N = (x, y - 1)
doStep (x, y) E = (x + 1, y)
doStep (x, y) S = (x, y + 1)
doStep (x, y) W = (x - 1, y)

nextDirection :: Direction -> Direction
nextDirection N = E
nextDirection E = S
nextDirection S = W
nextDirection W = N

part2 :: Grid -> Int
part2 x = findLoops guard updatedGrid (S.toList . S.delete (fst guard) . S.map fst . fst $ walkThrough guard updatedGrid S.empty)
  where
    guard =
      head . mapMaybe liftTupleMaybe . M.toList $
        M.map
          ( \p -> case p of
              Guard d -> pure d
              _ -> Nothing
          )
          x
    updatedGrid = M.insert (fst guard) Space x

findLoops :: ((Int, Int), Direction) -> Grid -> [(Int, Int)] -> Int
findLoops guard grid = foldl go 0
  where
    go :: Int -> (Int, Int) -> Int
    go n p = if snd (walkThrough guard (M.insert p Wall grid) S.empty) then n + 1 else n

liftTupleMaybe :: (a, Maybe b) -> Maybe (a, b)
liftTupleMaybe (a, Just b) = pure (a, b)
liftTupleMaybe (a, Nothing) = Nothing
