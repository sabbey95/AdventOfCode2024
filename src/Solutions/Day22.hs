module Solutions.Day22
  ( aoc22
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser, some, TokenParsing (token), integer)
import qualified Data.Bits as B
import Common.Debugging (traceLns)
import qualified Data.Map as M

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 $ MkAoCSolution parseInput part1
  printSolutions 22 $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = some $ token integer

part1 :: [Integer] -> Integer
part1 = sum . map (last . doMoves 2000)

doMoves :: Int -> Integer -> [Integer]
doMoves 0 i = [i]
doMoves n secretNumber = newNumber: doMoves (n-1) newNumber
  where
    step1 i = prune $ mix (64 * i) i
    step2 i = prune $ mix (i `div` 32) i
    step3 i = prune $ mix (2048 * i) i
    newNumber = foldl (\i f -> f i) secretNumber [step1, step2, step3]

part2 :: [Integer] -> Integer
part2 = maximum . M.elems . M.unionsWith (+) . map (toPathWeights . map (`mod` 10) . doMoves 2000)

toPathWeights :: [Integer] -> M.Map (Integer, Integer, Integer, Integer) Integer
toPathWeights (x:(y:(z:(a:(b:others))))) = M.insert (y-x,z-y,a-z,b-a) b $ toPathWeights (y:z:a:b:others)
toPathWeights _ = M.empty


mix:: Integer -> Integer -> Integer
mix = B.xor

prune:: Integer -> Integer
prune i = i `mod` 16777216