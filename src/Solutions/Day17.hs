{-# LANGUAGE TupleSections #-}

module Solutions.Day17
  ( aoc17,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Control.Lens ((^?), (^?!))
import Control.Lens.Traversal (element)
import Data.List (intercalate, findIndices)
import qualified Data.Map as M
import qualified Data.Bits as B
import Text.Trifecta (DeltaParsing (position), Parser, commaSep, integer, letter, some, string, token)
import Common.Debugging (traceLns)
import Text.Regex.TDFA (RegexContext(match))

aoc17 :: IO ()
aoc17 = do
  printSolutions 17 $ MkAoCSolution parseInput part1
  printSolutions 17 $ MkAoCSolution parseInput part2

type State = M.Map Char Integer

data Input = MkInput
  { state :: State,
    instructions :: [Integer],
    p :: Int,
    output :: [Integer]
  }
  deriving (Show)

parseInput :: Parser Input
parseInput = do
  registers <- M.fromList <$> some (token parseRegister)
  instructions <- parseInstructions
  pure (MkInput registers instructions 0 [])

parseInstructions :: Parser [Integer]
parseInstructions = do
  string "Program: "
  commaSep integer

parseRegister :: Parser (Char, Integer)
parseRegister = do
  string "Register "
  c <- letter
  string ": "
  (c,) <$> integer

part1 :: Input -> String
part1 = intercalate "," . map show . runInstructions

runInstructions :: Input -> [Integer]
runInstructions input = case (instruction, operand) of
  (Just i, Just o) -> runInstructions (updateState i o input)
  _ -> output input
  where
    instruction = instructions input ^? element (p input)
    operand = instructions input ^? element (p input + 1)

updateState :: Integer -> Integer -> Input -> Input
updateState instr operand (MkInput state is p output) =
  case instr of
    0 -> performAdvOp 'A'
    1 -> updateStateOnly (setRegisterValue 'B' (B.xor (getRegisterValue 'B') operand))
    2 -> updateStateOnly (setRegisterValue 'B' (comboOperandValue `mod` 8))
    3 -> MkInput state is (if getRegisterValue 'A' == 0 then p+2 else fromInteger operand) output
    4 -> updateStateOnly (setRegisterValue 'B' (B.xor (getRegisterValue 'B') (getRegisterValue 'C')))
    5 -> MkInput state is (p + 2) (output ++ [comboOperandValue `mod` 8])
    6 -> performAdvOp 'B'
    7 -> performAdvOp 'C'
    _ -> updateStateOnly state
  where
    comboOperandValue = getComboOperandValue operand getRegisterValue
    getRegisterValue c = M.findWithDefault 0 c state
    setRegisterValue c i = M.insert c i state
    updateStateOnly s = MkInput s is (p + 2) output
    performAdvOp c = updateStateOnly (setRegisterValue c ans)
      where
        ans = getRegisterValue 'A' `div` 2 ^ comboOperandValue

getComboOperandValue :: Integer -> (Char -> Integer) -> Integer
getComboOperandValue 4 s = s 'A'
getComboOperandValue 5 s = s 'B'
getComboOperandValue 6 s = s 'C'
getComboOperandValue i _ = i

part2 :: Input -> Integer
part2 = checkNext 1

checkNext :: Integer -> Input -> Integer
checkNext i (MkInput state is p output)
      | outState == is = i
      | length outState > length is = i
      | length outState < length is = checkNext (i * 8) (MkInput state is p output)
      | otherwise = checkNext (i + 8 ^ maximum matches) (MkInput state is p output)
      where
        newstate = M.insert 'A' i state
        outState = runInstructions (MkInput newstate is p output)
        matches = findIndices (uncurry (/=)) $ zip is outState
