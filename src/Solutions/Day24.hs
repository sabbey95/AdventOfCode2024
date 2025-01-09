{-# LANGUAGE TupleSections #-}

module Solutions.Day24
  ( aoc24,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging (traceLns)
import Data.List (intercalate, isPrefixOf, sort, sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Trifecta (CharParsing (anyChar, char, string), Parser, Parsing (try), TokenParsing (token), count, integer, letter, newline, some)
import Data.Bits (Bits(xor))

aoc24 :: IO ()
aoc24 = do
  printSolutions 24 $ MkAoCSolution parseInput part1
  printTestSolutions 24 $ MkAoCSolution parseInput part2

parseInput :: Parser (Input, Process)
parseInput = do
  initialValues <- some . try $ token parseInitialValue
  processes <- some $ token parseProcesses
  pure (M.fromList initialValues, M.fromList processes)

parseProcesses :: Parser (String, (String, Operation, String))
parseProcesses = do
  x <- parseNode
  char ' '
  operation <- parseOperation
  char ' '
  y <- parseNode
  string " -> "
  (,(x, operation, y)) <$> parseNode

parseOperation :: Parser Operation
parseOperation = do
  strOp <- some letter
  case strOp of
    "XOR" -> pure XOR
    "OR" -> pure OR
    "AND" -> pure AND
    s -> fail ("Unknown operation: " ++ s)

parseNode :: Parser String
parseNode = count 3 anyChar

parseInitialValue :: Parser (String, Integer)
parseInitialValue = do
  str <- parseNode
  string ": "
  (str,) <$> integer

data Operation = XOR | OR | AND deriving (Enum, Show, Eq, Ord)

type Input = M.Map String Integer

type Process = M.Map String (String, Operation, String)

part1 :: (Input, Process) -> Integer
part1 (input, process) =
    case output of
      Right i -> score 'z' i
      _ -> 0
  where output = finalInput input process

finalInput :: Input -> Process -> Either String Input
finalInput input process = fst $ foldl go (Right input, S.empty) (M.keys process)
  where
    go :: (Either String Input, S.Set String) -> String -> (Either String Input, S.Set String)
    go (Right i, previousNodes) node = case value of
      Just _ -> (Right i, newNodes)
      Nothing -> case newResult of
          Right i -> (Right i, newNodes) 
          Left s -> (Left s, newNodes)
      where
        value = M.lookup node i
        newNodes = S.insert node previousNodes
        (x, op, y) = M.findWithDefault ("", AND, "") node process
        newResult = do
                _  <- if node `S.member` previousNodes then Left "Not valid adder" else Right Nothing
                iX <- fst $ go (Right i, newNodes) x
                iY <- fst $ go (Right iX, newNodes) y
                let xVal = M.findWithDefault 0 x iX
                let yVal = M.findWithDefault 0 y iY
                pure $ M.insert node (result xVal op yVal) iY
    go s _ = s

result :: Integer -> Operation -> Integer -> Integer
result 1 AND 1 = 1
result _ AND _ = 0
result 0 OR 0 = 0
result _ OR _ = 1
result 0 XOR 1 = 1
result 1 XOR 0 = 1
result _ XOR _ = 0

score :: Char -> Input -> Integer
score c = fromBinary . map snd . getBinFromInput c

getBinFromInput :: Char -> Input -> [(String, Integer)]
getBinFromInput c = sortOn fst . M.toList . M.filterWithKey (\k _ -> [c] `isPrefixOf` k)

part2 :: (Input, Process) -> String
part2 (input, process) = display $ pairsToSwapFromRuleBreaks ++ [findLast allValidCombinations interestingCombinations]
  where
    freshInput = M.map (const 0) input
    rule1Breakers = M.keys $ M.filterWithKey (\k (x, op, y) -> "z" `isPrefixOf` k && k /= "z45" && op /= XOR) process
    rule2Breakers = M.keys $ M.filterWithKey (\k (x, op, y) -> not ("z" `isPrefixOf` k) && not (("x" `isPrefixOf` x && "y" `isPrefixOf` y) || ("x" `isPrefixOf` y && "y" `isPrefixOf` x)) && op == XOR) process
    pairsToSwapFromRuleBreaks = map (\r -> (r, matchingRuleBreaker (S.singleton r) rule2Breakers process)) rule1Breakers
    swappedProcessesFromRuleBreaks = foldl swapEm process pairsToSwapFromRuleBreaks
    interestingCombinations = [(score 'x' input, score 'y' input),(2 ^ 44, 2 ^ 44),(0, 0), (0, 2 ^ 44), (2 ^ 44, 0)]
    allNodes = filter (`notElem` (rule1Breakers ++ rule2Breakers)) (M.keys process)
    allValidCombinations = getAllValidCombinations allNodes process
    combinations = findLast allValidCombinations interestingCombinations
    findLast :: [(String, String)] -> [(Integer, Integer)] -> (String, String)
    findLast [x] _ = x
    findLast potentials [(x, y)] = findLast (filterCombinations potentials (x, y)) [(x - 1, y + 1)]
    findLast potentials (v:others) = findLast (filterCombinations potentials v) others
    filterCombinations::[(String, String)]  -> (Integer, Integer) -> [(String, String)]
    filterCombinations potentials (xVal, yVal) = filter (\p -> part1 (withYVal, swapEm swappedProcessesFromRuleBreaks p) == xVal + yVal) potentials
          where
            withXVal = replaceInInput 'x' xVal freshInput
            withYVal = replaceInInput 'y' yVal withXVal


display :: [(String, String)] -> String
display = intercalate "," . sort . concatMap (\(a, b) -> [a, b])

getAllValidCombinations :: [String] -> Process -> [(String, String)]
getAllValidCombinations [x] _ = []
getAllValidCombinations (x : xs) process = map (x,) xs ++ getAllValidCombinations xs process

matchingRuleBreaker :: S.Set String -> [String] -> Process -> String
matchingRuleBreaker found potentialMatches allProcesses
  | S.size foundPotentialMatches > 0 = S.elemAt 0 foundPotentialMatches
  | otherwise = matchingRuleBreaker nextTier potentialMatches allProcesses
  where
    foundList = S.toList found
    nextTier = S.unions [found, S.fromList (concatMap (parents allProcesses) foundList), S.fromList (concatMap (children allProcesses) foundList)]
    foundPotentialMatches = S.intersection nextTier (S.fromList potentialMatches)

parents :: Process -> String -> [String]
parents p s = maybe [] (\(x, _, y) -> [x, y]) $ M.lookup s p

children :: Process -> String -> [String]
children p s = M.keys $ M.filter (\(x, _, y) -> x == s || y == s) p

swapEm :: Process -> (String, String) -> Process
swapEm process (x, y) =
  M.insert x yVal (M.insert y xVal process)
  where
    xVal = M.findWithDefault (x, OR, y) x process
    yVal = M.findWithDefault (x, OR, y) y process

toBinary :: Integer -> [Integer]
toBinary 0 = []
toBinary n = toBinary (div n 2) ++ [mod n 2]

fromBinary :: [Integer] -> Integer
fromBinary = sum . zipWith (*) (iterate (* 2) 1)

replaceInInput :: Char -> Integer -> Input -> Input
replaceInInput charPrefix n input = fst $ foldr go (input, 0) binaryN
  where
    binaryN = toBinary n
    go :: Integer -> (Input, Int) -> (Input, Int)
    go digit (currentInput, index) = (M.insert (charPrefix : stringify index) digit currentInput, index + 1)

stringify :: Int -> String
stringify i = if length str < 2 then '0' : str else str
  where
    str = show i