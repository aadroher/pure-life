module PureLife.WorldSpec (spec) where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.Set (Set, empty, insert, member)
import PureLife.World (Cell, World(..), newWorld)
import Test.QuickCheck ((===))
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: forall r. Spec (random :: RANDOM | r) Unit
spec = describe "The cell world" do
    describe "some function" do
      pending "should satisfy some property"
