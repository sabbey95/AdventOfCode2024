{-# LANGUAGE TupleSections #-}
module Solutions.Day11
  ( aoc11
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser, integer, some)
import Data.Foldable (Foldable(toList))
import Common.Debugging (traceLns)
import qualified Data.Map as M
import Data.List (group)

aoc11 :: IO ()
aoc11 = do
  printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

parseInput :: Parser (M.Map Integer Integer)
parseInput = M.fromListWith (+) . map (,1) <$> some integer

part1 :: M.Map Integer Integer -> Integer
part1 = solve 25

blink :: Int -> M.Map Integer Integer -> M.Map Integer Integer
blink 0 stones = stones
blink n stones = blink (n - 1) . M.fromListWith (+) $ concatMap (\(v, c) -> map (,c) $ applyRules v) (M.toList stones)

applyRules :: Integer -> [Integer]
applyRules 0 = [1]
applyRules n
  | even (length str) =  map read [a, b]
  | otherwise = [n * 2024]
    where
      str = show n
      (a, b) = splitAt ((length str + 1) `div` 2) str

part2 ::M.Map Integer Integer -> Integer
part2 = solve 75

solve :: Int -> M.Map Integer Integer -> Integer
solve n = sum . M.elems . blink n