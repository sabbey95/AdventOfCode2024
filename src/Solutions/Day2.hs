module Solutions.Day2
  ( aoc2
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, integer', sepBy, whiteSpace, token, some, char, manyTill, eof)
import           Data.List           ( sort)

aoc2 :: IO ()
aoc2 = do
  printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2

type Report = [Integer]

parseInput :: Parser [Report]
parseInput = manyTill (token parseReport) eof

parseReport :: Parser Report
parseReport = sepBy integer' (char ' ')

part1 :: [Report] -> Int
part1 = length . filter isSafe

part2 :: [Report] -> Int
part2 = length . filter isFullySafe

isFullySafe:: Report -> Bool
isFullySafe report = isSafe report || any isSafe (subReports (length report - 1) report)

subReports:: Int -> Report -> [Report]
subReports 0 report = [removeN 0 report]
subReports n report = removeN n report:subReports (n-1) report

removeN :: Int -> [a] -> [a]
removeN _ [] = []
removeN 0 (x:xs) = xs
removeN n (x:xs) = x : removeN (n-1) xs

isSafe :: Report -> Bool
isSafe = checkNext . sortReport

sortReport :: Report -> Report
sortReport report
    | length report < 2 = report
    | head report > last report = reverse report
    | otherwise = report

checkNext :: Report  -> Bool
checkNext (x:xs)
    | null xs = True
    | head xs <= x || head xs > x + 3  = False
    | otherwise = checkNext xs