module PureLife.WorldSpec (spec) where

import Prelude
import Control.Monad.Eff.Random    (RANDOM)
import Test.QuickCheck ((===))
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: forall r. Spec (random :: RANDOM | r) Unit
spec = describe "The cell world" do
    it "does not do much" $ pure unit
    it "should fail" do
      shouldEqual true false
    pending "not implemented yet"
    it "can test with QucikCheck" do
      quickCheck \n -> n + n === 2 * n
