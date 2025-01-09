{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Solutions.Day3
  ( aoc3,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Text.Trifecta (CharParsing (anyChar, char, string), Parser, Parsing (eof, try), comma, integer, many, manyTill, parens, some)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1
  printSolutions 3 $ MkAoCSolution parseInput part2

type MulOperation = Either (Integer, Integer) Bool

parseInput :: Parser [MulOperation]
parseInput = do
  operations <- some $ try parseMulOperation <|> try parseDisabled <|> try parseEnabled <|> ignore
  pure $ catMaybes operations

ignore :: Parser (Maybe MulOperation)
ignore = anyChar $> Nothing

parseDisabled :: Parser (Maybe MulOperation)
parseDisabled = string "don't()" $> pure (Right False)

parseEnabled :: Parser (Maybe MulOperation)
parseEnabled = string "do()" $> pure (Right True)

parseMulOperation :: Parser (Maybe MulOperation)
parseMulOperation = do
  string "mul"
  op <- parens $ do
    a <- integer
    comma
    (a,) <$> integer
  pure $ pure (Left op)

part1 :: [MulOperation] -> Integer
part1 =
  sum
    . map
      ( \op -> case op of
          Left (a, b) -> a * b
          _ -> 0
      )

part2 :: [MulOperation] -> Integer
part2 = fst . foldl f (0, True)

f :: (Integer, Bool) -> MulOperation -> (Integer, Bool)
f (s, _) (Right enabled) = (s, enabled)
f (s, True) (Left (a, b)) = (s + a * b, True)
f state _ = state
