module Solutions.Day8
  ( aoc8,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Common.Geometry (enumerateMultilineString)
import Data.Foldable (Foldable (fold), maximumBy)
import Data.List (nub, tails)
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Data.Set as S
import Text.Trifecta (CharParsing (anyChar), Parser, anyChar, some)
import Foreign (toBool)

aoc8 :: IO ()
aoc8 = do
  printSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

type Grid = [((Int, Int), Char)]

parseInput :: Parser [((Int, Int), Char)]
parseInput = enumerateMultilineString <$> some anyChar

part1 :: Grid -> Int
part1 = solve False

solve :: Bool -> Grid -> Int
solve resonance grid = S.size $ foldl go S.empty antennaeCoords
  where
    antennae = filter (/= '.') . S.toList $ S.fromList (map snd grid)
    antennaeCoords = map (\a -> (a, map fst (filter (\b -> snd b == a) grid))) antennae
    allCoords = map fst grid
    maxCoord = (maximum (map fst allCoords), maximum (map snd allCoords))
    go :: S.Set (Int, Int) -> (Char, [(Int, Int)]) -> S.Set (Int, Int)
    go previous (_, coords) = foldl S.union previous (map (getAntinodes resonance maxCoord) (pairs coords))

getAntinodes :: Bool -> (Int, Int) -> ((Int, Int), (Int, Int)) -> S.Set (Int, Int)
getAntinodes resonance (limitX, limitY) ((ax, ay), (bx, by)) = if resonance then S.fromList antiNodesOnLine else S.fromList $ filter isWithinLimits [(ax + dx, ay + dy), (bx - dx, by - dy)]
  where
    isWithinLimits :: (Int, Int) -> Bool
    isWithinLimits (x, y) = x >= 0 && y >= 0 && x <= limitX && y <= limitY
    (dx, dy) = (ax - bx, ay - by)
    getAntinodesOnLine:: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    getAntinodesOnLine (vx, vy) (x, y) 
      | isWithinLimits (x + vx, y + vy) = (x, y) : getAntinodesOnLine (vx, vy) (x + vx, y + vy)
      | otherwise = [(x, y)]
    above = getAntinodesOnLine (dx, dy) (ax, ay)
    below = getAntinodesOnLine (-dx, -dy) (bx, by)
    antiNodesOnLine = above ++ below ++ [(ax, ay), (bx, by)]


pairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pairs l = filter (uncurry (/=)) [(x, y) | x <- l, y <- l]

part2 :: Grid -> Int
part2 = solve True
