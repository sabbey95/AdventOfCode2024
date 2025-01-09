{-# LANGUAGE TemplateHaskell #-}
module Common.Floyd where
import           Common.ListUtils (dropUntil)
import           Control.Lens     (makeLenses)
import           Data.Foldable    (find)
import           Data.List        (elemIndex)
import           GHC.OldList      (findIndex)
import Debug.Trace (traceM)

data CycleData a
  = MkCycleData
      { _index  :: !Int
      , _length :: !Int
      , _node   :: !a
      }
  deriving (Eq, Show)

makeLenses ''CycleData

hareAndTortoise :: (Eq b) =>
  (a -> a) -> --Function move from one node to the next
  a -> --Initial state
  (a -> b) -> --Function to compare if two nodes are equal
  Maybe (CycleData a)
hareAndTortoise f start eqFun = do
  let hare = drop 1 $ iterate (f . f) start
  let tortoise = drop 1 $ iterate f start
  let pairs = zip hare tortoise
  (t, h) <- find (uncurry areEqual) pairs --This will go forever unless I impose a limit of some sort
  (startIndex, (t', _)) <- find (\(_, y) -> uncurry areEqual y) $ zip [0..] $ zip (iterate f start) (iterate f h)
  length <- findIndex (areEqual t') $ drop 1 $ iterate f t' --This will be too low by 1 because I had to drop the first element
  pure $ MkCycleData startIndex (length + 1) t'
  where areEqual x y = eqFun x == eqFun y

