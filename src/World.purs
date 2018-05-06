module PureLife.World where

import Prelude

import Data.Set (Set, insert, fromFoldable)
import PureLife.Cell (Cell)

newtype World = World
  { aliveCells :: Set Cell
  }

instance showWorld :: Show World where
  show (World o) = "(w: " <> (show o.aliveCells) <> ")"

newWorld :: Array Cell -> World
newWorld cs = World
  { aliveCells: fromFoldable cs
  }

addCell :: Cell -> World -> World
addCell c (World o) = World
  { aliveCells: (insert c o.aliveCells)
  }
