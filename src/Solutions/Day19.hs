module Solutions.Day19
  ( aoc19,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging (traceLns)
import Data.Function (on)
import Data.List (isPrefixOf, nub, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Text.Trifecta (Parser, letter, many, newline, sepBy, some, string, token)

aoc19 :: IO ()
aoc19 = do
  printSolutions 19 $ MkAoCSolution parseInput part1
  printSolutions 19 $ MkAoCSolution parseInput part2

parseInput :: Parser Input
parseInput = do
  patterns <- sepBy (many letter) (string ", ")
  newline
  newline
  MkInput patterns <$> some (token (some letter))

data Input = MkInput
  { patterns :: [String],
    towels :: [String]
  }
  deriving (Show)

part1 :: Input -> Int
part1 (MkInput patterns towels) = length $ filter ((> 0) . fst . totalPaths M.empty patterns) towels

part2 :: Input -> Int
part2 (MkInput patterns towels) =
  sum $
    map
      (fst . totalPaths M.empty patterns)
      towels

totalPaths :: M.Map String Int -> [String] -> String -> (Int, M.Map String Int)
totalPaths cache patterns [] = (1, M.singleton [] 1)
totalPaths cache patterns towel
  | null matchingPatterns = (0, cache)
  | otherwise = foldl go (0, cache) matchingPatterns
  where
    matchingPatterns = filter (`isPrefixOf` towel) patterns
    go :: (Int, M.Map String Int) -> [Char] -> (Int, M.Map String Int)
    go (c, subCache) pattern = (c + n, M.insertWith (+) towel n backCache)
      where
        subString = drop (length pattern) towel
        cacheValue = M.lookup subString subCache
        (n, backCache) = case cacheValue of
          Nothing -> totalPaths subCache patterns subString
          Just n -> (n, subCache)
