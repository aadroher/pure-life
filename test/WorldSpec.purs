module PureLife.WorldSpec where

import Prelude
-- import Control.Monad.Aff (later')
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random    (RANDOM)
import Test.QuickCheck ((===), (/==))
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

spec :: forall e. StateT Aff (random :: RANDOM | e) Unit
spec =
    describe "The cell world" do
        it "does not do much" $ pure unit
        it "should fail" do
          shouldEqual true false
        -- pending "not implemented yet"
        it "can test with QucikCheck" do
          quickCheck \n -> n + n === 2 * n
        -- it "can fail with QucikCheck" do
        --   quickCheck \n -> n + n === 3 * n


-- main :: Eff (RunnerEffects ()) Unit
-- main = run [consoleReporter] do
--     describe "The cell world" do
--         pending "-- not many tests  yet"
