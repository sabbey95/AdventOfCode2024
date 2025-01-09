module Solutions.Day25
  ( aoc25
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, sepBy, many, some, token, anyChar, CharParsing (anyChar), newline, count, oneOf)
import Common.Geometry (Grid, enumerateMultilineStringToVectorMap, Point)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Linear (V2(..))
import Common.Debugging (traceLns)

aoc25 :: IO ()
aoc25 = do
  printSolutions 25 $ MkAoCSolution parseInput part1
  printSolutions 25 $ MkAoCSolution parseInput part2

parseInput :: Parser [Grid Char]
parseInput = map (enumerateMultilineStringToVectorMap . unlines) . chunksOf 7 <$> some (token (some (oneOf ".#")))

part1 :: [Grid Char] -> Int
part1 all = sum $ map goKeys keys
    where
      test = head all
      keys = filter (isRowFull 0) all
      locks = filter (isRowFull 6) all
      goKeys :: Grid Char -> Int
      goKeys key = length (filter fitsTogether locks)
        where
          keySpaces = M.keysSet $ M.filter (=='#') key
          fitsTogether :: Grid Char -> Bool
          fitsTogether lock = S.null $ S.intersection lockSpaces keySpaces
            where
              lockSpaces = M.keysSet $ M.filter (=='#') lock

part2 :: [Grid Char] -> String
part2 = undefined

row:: Int -> S.Set Point
row y = S.fromList ([V2 x y | x <- [0..6]])

isRowFull:: Int -> Grid Char -> Bool
isRowFull y g = notElem '.' . M.elems $ M.restrictKeys g (row y)