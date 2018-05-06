module PureLife.Cell where

import Prelude

newtype Position = Position
  { x :: Int
  , y :: Int
  }

instance eqPosition :: Eq Position where
  eq p0 p1 = eq (toArray p0) (toArray p1)

instance ordPosition :: Ord Position where
  compare p0 p1 = compare (toArray p0) (toArray p1)

instance showPosition :: Show Position where
  show (Position {x, y}) = "(" <> show x <> ", " <> show y <> ")"

toArray :: Position -> Array Int
toArray (Position {x, y}) = [x, y]

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
  show Alive = "alive"
  show Dead = "dead"

newtype Cell = Cell
  { position :: Position
  , state :: State
  }

instance eqCell :: Eq Cell where
  eq (Cell o0) (Cell o1) =
    o0.position == o1.position && o0.state == o1.state

instance ordCell :: Ord Cell where
  compare (Cell o0) (Cell o1) =
    let
      p0 = o0.position
      p1 = o1.position
    in
      if p0 /= p1 then
        compare p0 p1
      else
        compare o0.state o1.state

instance showCell :: Show Cell where
  show (Cell {position, state}) =
    "(c: " <> show position <> ", " <> show state <> ")"

newCell :: Int -> Int -> State -> Cell
newCell x y s = Cell
  { position: Position
    { x: x
    , y: y
    }
  , state: s
  }

isAlive :: Cell -> Boolean
isAlive (Cell {position: _, state: Alive}) = true
isAlive (Cell {position: _, state: Dead}) = false
