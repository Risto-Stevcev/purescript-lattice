module Test.Data.Lattice where

import Data.Lattice.Verify
import Control.Monad.Eff (Eff)
import Prelude (Unit, discard, ($))
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Type.Proxy (Proxy(..))

main ∷ Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Lattice Boolean" do
    it "should satisfy the laws for Lattice" do
      quickCheck' 1000 $ verify (Proxy ∷ Proxy Boolean) verifyLattice
  describe "BoundedLattice Boolean" do
    it "should satisfy the laws for BoundedLattice" do
      quickCheck' 1000 $ verify (Proxy ∷ Proxy Boolean) verifyBoundedLattice
