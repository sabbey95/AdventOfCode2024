module Main where

import qualified Data.IntMap     as IM
import           Solutions.Day1  (aoc1)
import           Solutions.Day10 (aoc10)
import           Solutions.Day11 (aoc11)
import           Solutions.Day12 (aoc12)
import           Solutions.Day13 (aoc13)
import           Solutions.Day14 (aoc14)
import           Solutions.Day15 (aoc15)
import           Solutions.Day16 (aoc16)
import           Solutions.Day17 (aoc17)
import           Solutions.Day18 (aoc18)
import           Solutions.Day19 (aoc19)
import           Solutions.Day2  (aoc2)
import           Solutions.Day20 (aoc20)
import           Solutions.Day21 (aoc21)
import           Solutions.Day22 (aoc22)
import           Solutions.Day23 (aoc23)
import           Solutions.Day24 (aoc24)
import           Solutions.Day25 (aoc25)
import           Solutions.Day3  (aoc3)
import           Solutions.Day4  (aoc4)
import           Solutions.Day5  (aoc5)
import           Solutions.Day6  (aoc6)
import           Solutions.Day7  (aoc7)
import           Solutions.Day8  (aoc8)
import           Solutions.Day9  (aoc9)

import System.TimeIt (timeIt, timeItNamed)
import Text.Printf (printf)

funs =
  [ aoc1,
    aoc2,
    aoc3,
    aoc4,
    aoc5,
    aoc6,
    aoc7,
    aoc8,
    aoc9,
    aoc10,
    aoc11,
    aoc12,
    aoc13,
    aoc14,
    aoc15,
    aoc16,
    aoc17,
    aoc18,
    aoc19,
    aoc20,
    aoc21,
    aoc22,
    aoc23,
    aoc24,
    aoc25
  ]

main :: IO ()
main = do
  timeItNamed "********\nOVERALL TIME" $ runSolutions funs

runSolutions :: [ IO ()] -> IO ()
runSolutions fns
  | length fns == 1 = timeIt $ head fns
  | otherwise = do
      timeIt $ head fns
      printf "\n"
      runSolutions $ tail fns