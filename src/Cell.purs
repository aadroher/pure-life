module PureLife.Cell
    ( Coordinates(..)
    , Cell(..)
    , State(..)
    ) where

import Prelude

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

data Coordinates = Coordinates
    { x :: Int
    , y :: Int
    }

-- coordRecordToArray :: forall f. f -> (f -> Coordinates -> Coordinates) -> (f -> Array Int -> Array Int)
coordRecordToArray :: forall f. (Array Int -> Array Int -> f) -> Coordinates -> Coordinates -> f
coordRecordToArray f (Coordinates {x: x0, y: y0}) (Coordinates {x: x1, y: y1}) =
    f [x0, x1] [y0, y1]

instance eqCoordinates :: Eq Coordinates where
  eq (Coordinates {x: x0, y: y0}) (Coordinates {x: x1, y: y1}) =
        eq [x0, x1] [y0, y1]

instance ordCoordinates :: Ord Coordinates where
  compare (Coordinates {x: x0, y: y0}) (Coordinates {x: x1, y: y1}) =
    compare [x0, x1] [y0, y1]

data Cell = Cell Coordinates State

instance eqCell :: Eq Cell where
  eq (Cell t0 s0) (Cell t1 s1) = t0 == t1 && s0 == s1

instance ordCell :: Ord Cell where
  compare (Cell t0 s0) (Cell t1 s1) =
    case compare t0 t1 of
        EQ -> compare s0 s1
        _ -> compare t0 t1

instance showCell :: Show Cell where
  show (Cell c s) =
    case c of
        Coordinates {x, y} ->
            "(" <> show x <> ", " <> show y <> "): " <> show s
