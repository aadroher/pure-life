module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Node.FS (FS)
import Test.Spec.Discovery (discover)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Eff (QCRunnerEffects (fs :: FS)) Unit
main = discover "PureLife\\..+Spec" >>= run [consoleReporter]
