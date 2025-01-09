module Solutions.Day7
  ( aoc7
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser, manyTill, token, eof, sepBy, integer', char, CharParsing (string))
import qualified Data.Map as M

aoc7 :: IO ()
aoc7 = do
  printSolutions 7 $ MkAoCSolution parseInput part1
  printSolutions 7 $ MkAoCSolution parseInput part2

data Operation = MkOperation {
  result:: Integer,
  inputs:: [Integer]
} deriving (Eq, Show)

data Op = Addition | Multiplication | Concatenation deriving (Enum)

parseInput :: Parser [Operation]
parseInput = manyTill (token parseReport) eof

parseReport :: Parser Operation
parseReport = do
  result <- integer'
  string ": "
  MkOperation result <$> sepBy integer' (char ' ')

isValidOperation :: [Op] -> Operation ->  Bool
isValidOperation possOps o =  elem (result o) . foldl go [] $ inputs o
  where
    go :: [Integer] -> Integer -> [Integer]
    go [] n = [n]
    go results n = concatMap (\r -> map (applyOperation r n) possOps) $ filter (<= result o) results

applyOperation :: Integer -> Integer -> Op -> Integer
applyOperation s n Addition =  s + n
applyOperation s n Multiplication =  s * n
applyOperation s n Concatenation = combineAsString s n

combineAsString :: Integer -> Integer -> Integer
combineAsString n m = read (show n ++ show m)


part1 :: [Operation] -> Integer
part1 = solve [ Addition, Multiplication]

solve :: [Op] -> [Operation] -> Integer
solve ops = sum . map result . filter (isValidOperation ops)

part2 :: [Operation] -> Integer
part2 = solve [ Addition, Multiplication, Concatenation]
