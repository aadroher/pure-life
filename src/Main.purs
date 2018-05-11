module Main (main) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (mapMaybe)
import Data.Set (Set)
import Data.Set (fromFoldable) as S

-- oscillatorCellPositions :: Array (Array Int)
-- oscillatorCellPositions =
--   [ [0, 0]
--   , [0, 1]
--   , [0, 2]
--   ]
--
-- cells :: Set Cell
-- cells = S.fromFoldable $ mapMaybe fromPair oscillatorCellPositions
--
-- aCell :: Cell
-- aCell = newCell 0 0

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    log "Hello (cell) world!"
  -- let w0 = newWorld []
  -- log $ "Empty world: " <> (show w0)
  -- logShow aCell
  -- let w1 = World cells
  -- log $ "Filled world: " <> (show w1)
  -- logShow $ World $ affectedCells w1
