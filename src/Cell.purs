module PureLife.Cell
    ( Cell(..)
    , State(..)
    ) where

import Prelude

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

newtype Coordinates = Tuple (Int Int)

data Cell = Cell Coordinates State

instance eqCell :: Eq Cell where
  eq (Cell t0) (Cell t1) = t0 == t1

instance ordCell :: Ord Cell where
  compare (Cell t0) (Cell t1) = compare t0 t1

instance showCell :: Show Cell where
  show (Cell (Tuple x y) s) =
    "(" <> show x <> ", " <> show y <> ")"
