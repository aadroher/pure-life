module PureLife.World where

import Prelude

import Data.Set (Set, fromFoldable, insert, member)
import Data.Tuple (Tuple(..))

data Cell = Cell (Tuple Int Int)

instance eqCell :: Eq Cell where
  eq (Cell t0) (Cell t1) = t0 == t1

instance ordCell :: Ord Cell where
  compare (Cell t0) (Cell t1) = compare t0 t1

instance showCell :: Show Cell where
  show (Cell (Tuple x y)) =
    "(" <> show x <> ", " <> show y <> ")"

newCell :: Int -> Int -> Cell
newCell x y = Cell (Tuple x y)

neighbours :: Cell -> Set Cell
neighbours (Cell (Tuple x y)) =
  fromFoldable $
    mapper <$> adjacentPositions
  where
    adjacentPositions =
      [ [-1, 1],  [0, 1],  [1, 1]
      , [-1, 0],           [1, 0]
      , [-1, -1], [0, -1], [1, -1]
      ]
    mapper :: Array Int -> Cell
    mapper [i, j] = newCell (x + i) (y + j)
    mapper _ = newCell 0 0

data World = World (Set Cell)

instance showWorld :: Show World where
  show (World aliveCells) = "(w: " <> (show aliveCells) <> ")"

newWorld :: Array Cell -> World
newWorld cs = World $ fromFoldable cs

addCell :: Cell -> World -> World
addCell cell (World aliveCells) = World $ insert cell aliveCells

isAlive :: Cell -> World -> Boolean
isAlive c (World cs) = member c cs

affectedCells :: World -> Set Cell
affectedCells (World cs) =



evolve :: World -> World
evolve w = w
