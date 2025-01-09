{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Solutions.Day9
  ( aoc9,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.ListUtils (singleton)
import Data.List (findIndices)
import Data.List.Split (chunk, chunksOf)
import Data.Text (replace)
import Numeric.Lens (base)
import Text.Trifecta (Parser, digit, many)

aoc9 :: IO ()
aoc9 = do
  printTestSolutions 9 $ MkAoCSolution parseInput part1
  printTestSolutions 9 $ MkAoCSolution parseInput part2

data Position = Space | Block Integer deriving (Show)

parseInput :: Parser [(Int, Position)]
parseInput = do
  digits <- many digit
  let digitsInt = map (read . singleton) digits
  let chunks = zip [0 ..] $ toPairs digitsInt
  pure $ concatMap (\(id, (blocks, breaks)) -> [(blocks, Block id), (breaks, Space)]) chunks

toPairs :: [Int] -> [(Int, Int)]
toPairs [] = []
toPairs [x] = [(x, 0)]
toPairs (x : y : xs) = (x, y) : toPairs xs

part1 :: [(Int, Position)] -> Integer
part1 = checkSum . reducePositions . concatMap (uncurry replicate)

reducePositions :: [Position] -> [Position]
reducePositions positions = fst $ foldl go ([], (length positionsIndexed, filter (isBlock . snd) positionsIndexed)) positionsIndexed
  where
    positionsIndexed = zip [0 ..] positions
    go :: ([Position], (Int, [(Int, Position)])) -> (Int, Position) -> ([Position], (Int, [(Int, Position)]))
    go (ps, (n, current)) (i, Block next)
      | n <= i = (ps, (n, current))
      | otherwise = (ps ++ [Block next], (n, current))
    go (ps, (n, current)) (i, Space)
      | n <= i = (ps, (n, current))
      | otherwise = case lastBlock of
          Nothing -> (ps, (n, filteredList))
          Just (newN, newP) -> (ps ++ [newP], (newN, filteredList))
      where
        filteredList = filter (\(ind, _) -> ind < n) current
        lastBlock = lastOption filteredList

checkSum :: [Position] -> Integer
checkSum = sum . zipWith (*) [0 ..] . map value

value :: Position -> Integer
value Space = 0
value (Block i) = i

isBlock :: Position -> Bool
isBlock Space = False
isBlock (Block _) = True

lastOption :: [a] -> Maybe a
lastOption [] = Nothing
lastOption x = Just $ last x

part2 :: [(Int, Position)] -> Integer
part2 = solve moveFullBlock


solve :: MakeMove -> [(Int, Position)] -> Integer
solve makeMove = checkSum . concatMap (uncurry replicate) . reverse . moveBlocks makeMove . reverse

moveBlocks :: MakeMove -> [(Int, Position)] -> [(Int, Position)]
moveBlocks  _ [] = []
moveBlocks replaceBlock ((n, Space) : others) = (n, Space) : moveBlocks replaceBlock others
moveBlocks replaceBlock ((n, b) : others) = case matchingBlock of
    Nothing -> (n, b): moveBlocks replaceBlock others
    Just p -> (n, Space) : moveBlocks replaceBlock (replaceBlock others p n b)
  where
    matchingBlock =
      lastOption $ findIndices
        ( \a -> case a of
            (_, Block b) -> False
            (nSpace, Space) -> n <= nSpace
        ) others

type MakeMove = [(Int, Position)] -> Int -> Int -> Position -> [(Int, Position)]
moveFullBlock :: MakeMove
moveFullBlock positions index numberToReplace p = replaceN index (newSpaces ++ [(numberToReplace, p)]) positions
    where
      (nSpace, _) = positions!!index
      newSpaces = [(nSpace - numberToReplace, Space) | nSpace > numberToReplace]


replaceN :: Int -> [a] -> [a] -> [a]
replaceN _ with [] = []
replaceN 0 with (x:xs) = with ++ xs
replaceN n with (x:xs) = x : replaceN (n-1) with xs