{-# LANGUAGE TupleSections #-}
module Solutions.Day12
  ( aoc12,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging (traceLns)
import Common.Geometry (Grid, Point, allOrthogonalNeighbours, enumerateMultilineStringToVectorMap, neighbours, renderVectorSet, renderVectorMap)
import Data.Foldable (minimumBy)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Set as S
import Foreign (toBool)
import Linear (V2 (..))
import Linear.Affine (_Point)
import Text.Trifecta (Parser, anyChar, some)
import Data.IntMap (member)

aoc12 :: IO ()
aoc12 = do
  printSolutions 12 $ MkAoCSolution parseInput part1
  printSolutions 12 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> some anyChar

type ScoreArea = S.Set Point -> Int

solve :: ScoreArea -> Grid Char -> Int
solve scoreArea = sum . map scoreArea . getAreas

scoreAreaPt1 :: ScoreArea
scoreAreaPt1 points = area * perimiter
  where
    area = S.size points
    perimiter = length (getPerimeter points allOrthogonalNeighbours)

getPerimeter :: S.Set Point -> (Point -> S.Set Point) -> [Point]
getPerimeter p neighbours =
  concatMap
    (filter (`notElem` p))
    (S.toList $ S.map (S.toList . neighbours) p)

getAreas :: Grid Char -> [S.Set Point]
getAreas g = if M.size g == 0 then [] else section : getAreas (M.filterWithKey (\p _ -> p `S.notMember` section) g)
  where
    firstPoint = head . M.toList $ M.take 1 g
    section = allNeighbours g (S.singleton (fst firstPoint)) (snd firstPoint)

allNeighbours :: Grid Char -> S.Set Point -> Char -> S.Set Point
allNeighbours grid points c =
  if S.size newPointsInGarden == 0
    then points
    else allNeighbours grid (S.union points newPointsInGarden) c
  where
    allNeighbourPoints = S.foldl S.union S.empty (S.map allOrthogonalNeighbours points)
    newPoints = S.difference allNeighbourPoints points
    gridPoints = M.filter (== c) $ M.restrictKeys grid newPoints
    newPointsInGarden = M.keysSet gridPoints

part1 :: Grid Char -> Int
part1 = solve scoreAreaPt1

part2 :: Grid Char -> Int
part2 = solve scoreAreaPt2 . M.filter (/= '.')

scoreAreaPt2 :: ScoreArea
scoreAreaPt2 allPoints = area * nSides
  where
    area = length allPoints
    perimeter = getPerimeter allPoints allOrthogonalNeighbours
    perimiters = S.fromList $ concatMap (\(V2 xP yP) -> S.toList $ S.map (\(V2 xWall yWall) -> (V2 xP yP,(xWall - xP,  yWall - yP))) $ S.intersection allPoints (allOrthogonalNeighbours (V2 xP yP))) (S.fromList perimeter)
    (lonely, n ) = foldl go (perimiters, 0) perimiters
    nSides = length lonely + n
    go :: (S.Set (Point, (Int, Int)), Int) -> (Point, (Int, Int)) -> (S.Set (Point, (Int, Int)), Int)
    go (left, count) p = if S.size sidePoints <= 1 then (left, count) else (S.difference left sidePoints, count + 1)
      where
        sidePoints = getSides (S.singleton p) 
        getSides :: S.Set (Point, (Int, Int)) -> S.Set (Point, (Int, Int))
        getSides points = if S.null newPoints then points else getSides (S.union points newPoints)
          where
            allNeighbourPoints = S.intersection left $ S.foldl S.union S.empty (S.map (\(p, d) -> S.map (,d) (allOrthogonalNeighbours p)) points)
            newPoints =  S.difference allNeighbourPoints points


removeAll :: [Point] -> S.Set Point -> [Point]
removeAll = S.fold removeP

removeP :: Point -> [Point] -> [Point]
removeP p [] = []
removeP p (x:xs)
  | x == p = xs
  | otherwise = x : removeP p xs
