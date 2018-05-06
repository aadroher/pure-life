module PureLife.World where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, head, singleton, tail, (..), (:))
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

data Cell = Cell (Tuple Int Int) State

instance eqCell :: Eq Cell where
  eq (Cell t0 s0) (Cell t1 s1) = t0 == t1 && s0 == s1

instance ordCell :: Ord Cell where
  compare (Cell t0 s0) (Cell t1 s1) =
    case firstComparison of
      EQ -> firstComparison
      _ -> compare s0 s1
    where firstComparison = compare t0 t1

instance showCell :: Show Cell where
  show (Cell (Tuple x y) s) =
    "[(" <> show x <> ", " <> show y <> "):" <> show s <> "]"

newCell :: Int -> Int -> State -> Cell
newCell x y s = Cell (Tuple x y) s

fromPair :: Array Int -> Maybe Cell
fromPair [x, y] = Just $ newCell x y Alive
fromPair _ = Nothing

neighbours :: Cell -> World -> Set Cell
neighbours c w =
  S.fromFoldable adjacentPositions
  where
    adjacentPositions = do
      let (Cell (Tuple x y) _) = c
      let unitRange = -1 .. 1
      i <- unitRange
      j <- unitRange
      guard $ (i /= 0 || j /= 0)
      let cellX = (x + i)
          cellY = (y + j)
      pure $ newCell (x + i) (y + j) $ cellState c w

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
