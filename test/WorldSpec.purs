module PureLife.WorldSpec where

import Prelude
-- import Control.Monad.Aff (later')
import Control.Monad.Eff (Eff)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

spec :: forall r. Spec r Unit
spec =
    describe "The cell world" do
        it "does not do much" $ pure unit


-- main :: Eff (RunnerEffects ()) Unit
-- main = run [consoleReporter] do
--     describe "The cell world" do
--         pending "-- not many tests  yet"
