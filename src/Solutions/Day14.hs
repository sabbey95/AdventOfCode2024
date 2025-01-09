
module Solutions.Day14
  ( aoc14
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser, some, token, CharParsing (string), integer, comma, Errable (raiseErr))
import qualified Data.Bifunctor
import Common.Debugging (traceLns)
import Common.Geometry (renderVectorSet)
import Linear (V2(..))
import qualified Data.Set as S


aoc14 :: IO ()
aoc14 = do
  printSolutions 14 $ MkAoCSolution parseInput part1
  printSolutions 14 $ MkAoCSolution parseInput part2

data Robot = MkRobot {
  position:: (Integer, Integer),
  velocity:: (Integer, Integer)
} deriving (Show)

parseInput :: Parser [Robot]
parseInput = some $ token parseRobot

parseRobot :: Parser Robot
parseRobot = do
  string "p="
  px <- integer
  comma
  py <- integer
  string "v="
  vx <- integer
  comma
  vy <- integer
  pure $ MkRobot (px, py) (vx,vy)

part1 :: [Robot] -> Int
part1 = solve 100 (101, 103)

solve :: Integer -> (Integer, Integer) -> [Robot] -> Int
solve moves limits = getScore limits . map (move moves limits)

getScore :: (Integer, Integer) -> [Robot] -> Int
getScore (limitX, limitY) robots = product $ map (length . matchingRobots robots quadrants) [((<),(<)),((<),(>)),((>),(<)),((>), (>))]
  where quadrants = (limitX `div` 2, limitY `div` 2)

looksLikeTree :: (Integer, Integer) -> [Robot] -> Bool
looksLikeTree (limitX, limitY) robots = limitY `div` 5 < toInteger (length (filter (\(MkRobot (px, py) _) -> px == qx ) robots))
  where (qx, qy) = (limitX `div` 2, limitY `div` 2)

matchingRobots :: [Robot] -> (Integer, Integer) -> (Integer -> Integer -> Bool, Integer -> Integer -> Bool) -> [Robot]
matchingRobots robots (quadrantMarkerX, quadrantMarkerY) (checkX, checkY) = filter (\(MkRobot (px, py) _) -> checkX px quadrantMarkerX && checkY py quadrantMarkerY) robots

move :: Integer -> (Integer, Integer) -> Robot -> Robot
move n limits (MkRobot (px, py) (vx,vy)) =
   MkRobot (Data.Bifunctor.bimap (mod (px + (vx * n))) (mod (py + (vy * n))) limits) (vx, vy)

part2 :: [Robot] -> Int
part2 ro = getScore limits r
  where
    limits = (101, 103)
    r = moveAll 1 limits (getScore limits ro) ro

moveAll::Integer-> (Integer, Integer) ->Int  -> [Robot] -> [Robot]
moveAll n limits minSafety r
  | n > uncurry (*) limits = r
  | 1110 < n && n < 1125 = 
    moveAll (n+1) limits minSafety newRobots
  | otherwise = if newSafety < minSafety then
    moveAll (n+1) limits newSafety newRobots else moveAll (n+1) limits minSafety newRobots
    where
      newRobots = map (move 1 limits) r
      newSafety = getScore limits newRobots
