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
part1 (input, process) = score 'z' (finalInput input process)

finalInput :: Input -> Process -> Input
finalInput input process = foldl go input (M.keys process)
  where
    go :: Input -> String -> Input
    go i node = case value of
      Just _ -> i
      Nothing -> M.insert node (result xVal op yVal) iY
      where
        value = M.lookup node i
        (x, op, y) = M.findWithDefault ("", AND, "") node process
        iX = go i x
        iY = go iX y
        xVal = M.findWithDefault 0 x iX
        yVal = M.findWithDefault 0 y iY

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
part2 (input, process) = foldl go [] maybes
  where
    -- rule1 = M.keys $ M.filterWithKey (\k (x, op, y) -> "z" `isPrefixOf` k && k /= "z45" && op /= XOR) process
    -- rule2 = M.keys $ M.filterWithKey (\k (x, op, y) -> not ("z" `isPrefixOf` k) && not (("x" `isPrefixOf` x && "y" `isPrefixOf` y) || ("x" `isPrefixOf` y && "y" `isPrefixOf` x)) && op == XOR) process
    defos = [("z08", "vvr"), ("z28", "tfb"), ("z39", "mqh")]
    go :: String -> (String, String) -> String
    go s a = if value /= expectedZ then s ++ " : " ++ show a else 
      traceLns (intercalate "," . sort $ concatMap (\(a, b)-> [a,b]) (defos ++ [a]))
      s
        where
          swappedProcesses = foldl swapEm process (defos ++ [a])
          value = part1 (input, swappedProcesses)


    -- swappedProcesses = foldl swapEm process defos
    xVal = score 'x' input
    yVal = score 'y' input
    expectedZ =
      xVal
        + yVal
    -- allInputs = finalInput input process
    -- allNodes = filter (`notElem` (rule1 ++ rule2)) (M.keys process)
    -- go :: String -> S.Set String
    -- go s = case M.lookup s process of
    --   Nothing -> S.singleton s
    --   Just (x, _, y) -> S.unions [S.fromList [s, x, y], go x, go y]
--     run :: [String] -> String
--     run [x] = ""
--     run (x : xs) = run2 x xs ++ run xs
--     run2 :: String -> [String] -> String
--     run2 x [] = ""
--     run2 x (y : ys)
--       | isDodgy go (x, y) swappedProcesses = run2 x ys
--       | value == expectedZ = (str ++ " : ") ++ run2 x ys
--       | otherwise = run2 x ys
--       where
--         swapped =
--           -- traceLns (show (x ++ "," ++ y)) 
--           swapEm swappedProcesses (x, y)
--         value = part1 (input, swapped)
--         str = traceLns (show (value - expectedZ, "Answer: " ++ x ++ "," ++ y)) y ++ "," ++ x

-- isDodgy :: (String -> S.Set String) -> (String, String) -> Process -> Bool
-- isDodgy getEm (x, y) process = S.member (x, y) dodgyOnes || S.member x dodge||S.member y dodge|| S.member x (getEm y) || S.member y (getEm x)
--   where
--     dodgyOnes =
--       S.fromList
--         [ ("bhv", "ctv"),
--           ("bhv", "kwv"),
--           ("bkk", "ctv"),
--           ("bkk", "kwv"),
--           ("bkk", "ctv"),
--           ("bpq", "ctv"),
--           ("brn", "ctv"),
--           ("brn", "kwv"),
--           ("ckr", "ctv"),
--           ("ckr", "kwv"),
--           ("ctv","cvn"),
--           ("cvn","kwv"),
--           ("djn","kwv"),
--           ("fcn","kwv"),
--           ("fcq","kwv"),
--           ("bpq","kwv"),
--           ("fdd","kwv"),
--           ("fpn","kwv"),
--           ("fpw","kwv"),
--           ("fpw","kwv"),
--           ("fvq","kwv"),
--           ("fdd","kwv")
--         ]
--     dodge =
--         S.fromList
--           [ "ctv",
--             "kwv"
--           ]

-- (xX, _, yX) = M.findWithDefault (x, OR, y) x process
-- (xY, _, yy) = M.findWithDefault (x, OR, y) y process

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


maybes:: [(String, String)]
maybes = [("gms","bkr"),
  ("pvv","bkr"),
  ("rnq","bkr")]