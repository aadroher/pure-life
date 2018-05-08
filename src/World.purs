module PureLife.World where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filterA, head, singleton, tail, (..), (:))
import Data.Array (fromFoldable) as A
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty, insert, member, union)
import Data.Set (fromFoldable) as S
import Data.Tuple (Tuple(..))

data State = Alive | Dead

instance eqState :: Eq State where
  eq Alive Alive = true
  eq Dead Dead = true
  eq _ _ = false

instance ordState :: Ord State where
  compare Dead Alive = LT
  compare Alive Dead = GT
  compare _ _ = EQ

instance showState :: Show State where
  show Dead = "💀"
  show Alive = "❤️"

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

fromPair :: Array Int -> Maybe Cell
fromPair [x, y] = Just $ newCell x y
fromPair _ = Nothing

neighbours :: Cell -> Set Cell
neighbours (Cell (Tuple x y)) =
  S.fromFoldable adjacentPositions
  where
    adjacentPositions = do
      let unitRange = -1 .. 1
      i <- unitRange
      j <- unitRange
      guard $ (i /= 0 || j /= 0)
      pure $ newCell (x + i) (y + j)

data World = World (Set Cell)

instance showWorld :: Show World where
  show (World aliveCells) = "(w: " <> (show aliveCells) <> ")"

newWorld :: Array Cell -> World
newWorld cs = World $ S.fromFoldable cs

addCell :: Cell -> World -> World
addCell cell (World aliveCells) = World $ insert cell aliveCells

isAlive :: Cell -> World -> Boolean
isAlive c (World cs) = member c cs

cellState :: Cell -> World -> State
cellState c w =
  case isAlive c w of
    true -> Alive
    false -> Dead

affectedCells :: World -> Set Cell
affectedCells (World cellSet) =
  S.fromFoldable newCells
  where
    cells = A.fromFoldable cellSet
    newCells = cells >>= (A.fromFoldable <<< neighbours)





-- rule1 :: Cell -> World -> Boolean
-- rule1 c (World cellSet) =

-- shouldDie :: Cell -> World -> Boolean
-- shouldDie c (World cellSet) =
--   isAlive c w && ((size cellSet) < 2 ||


-- evolve :: World -> World
-- evolve w = w
