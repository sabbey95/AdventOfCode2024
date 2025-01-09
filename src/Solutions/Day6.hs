
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Solutions.Day6
  ( aoc6,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Text.Trifecta (CharParsing (anyChar), Parser, letter, some, token, try)
import Common.Debugging (traceLns)

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

data GridPoint = Wall | Space | Guard Direction deriving (Eq, Show)

data Direction = N | S | E | W deriving (Enum, Eq, Show)

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
part1 x = S.size . S.fromList . map fst . fst $ walkThrough guard x []
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


walkThrough :: ((Int, Int), Direction) -> Grid -> [((Int, Int), Direction)] -> ([((Int, Int), Direction)], Bool)
walkThrough (guardP, guardD) grid result = if (guardP, guardD) `elem` result then (result, True) else newPath
  where
    endPoint = doStep guardP guardD
    newPath = case M.lookup endPoint grid of
      Just Wall -> walkThrough (guardP, nextDirection guardD) grid result
      Just Space -> walkThrough (endPoint, guardD) grid $ result ++ [(guardP, guardD)]
      _ -> (result ++ [(guardP, guardD)], False)

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
part2 x = findLoops guard updatedGrid (S.toList . S.fromList . map fst . fst $ walkThrough guard updatedGrid [])
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

findLoops:: ((Int, Int), Direction) -> Grid -> [(Int, Int)] -> Int
findLoops guard grid = foldl go 0
  where
    go :: Int -> (Int, Int) -> Int
    go n p = if p1 /= fst guard && snd (walkThrough guard (M.insert p Wall grid) []) then n + 1 else n
      where 
        p1 = 
          traceLns (show n)
            p
        

liftTupleMaybe :: (a, Maybe b) -> Maybe (a, b)
liftTupleMaybe (a, Just b) = pure (a, b)
liftTupleMaybe (a, Nothing) = Nothing
