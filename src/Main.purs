module Main (main) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import PureLife.Cell (Cell, State(Alive), newCell)
import PureLife.World (addCell, newWorld)

aCell :: Cell
aCell = newCell 0 1 Alive

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let w0 = newWorld []
  log $ "Empty world: " <> (show w0)
  logShow aCell
  let w1 = addCell aCell w0
  log $ "Filled world: " <> (show w1)
