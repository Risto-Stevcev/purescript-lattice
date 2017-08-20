module Test.Data.Lattice where

import Data.Lattice
import Control.Algebra.Properties (absorbtion, associative, commutative,
                                   idempotent', identity)
import Control.Monad.Eff (Eff)
import Prelude (class Eq, Unit, discard, ($), (&&))
import Test.QuickCheck (class Arbitrary)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Type.Proxy (Proxy(..))


verifySemilattice ∷ ∀ a. Eq a ⇒ Arbitrary a ⇒ (a → a → a) → a → a → a → Boolean
verifySemilattice f a b c =
  associative f a b c && commutative f a b && idempotent' f a

verifyJoinSemilattice
  ∷ ∀ a. JoinSemilattice a ⇒ Eq a ⇒ Arbitrary a
  ⇒ Proxy a → a → a → a → Boolean
verifyJoinSemilattice _ = verifySemilattice join

verifyMeetSemilattice
  ∷ ∀ a. MeetSemilattice a ⇒ Eq a ⇒ Arbitrary a
  ⇒ Proxy a → a → a → a → Boolean
verifyMeetSemilattice _ = verifySemilattice meet

verifyBoundedJoinSemilattice
  ∷ ∀ a. BoundedJoinSemilattice a ⇒ Eq a ⇒ Arbitrary a
  ⇒ Proxy a → a → a → a → Boolean
verifyBoundedJoinSemilattice p a b c =
  verifyJoinSemilattice p a b c && identity join bottom a

verifyBoundedMeetSemilattice
  ∷ ∀ a. BoundedMeetSemilattice a ⇒ Eq a ⇒ Arbitrary a
  ⇒ Proxy a → a → a → a → Boolean
verifyBoundedMeetSemilattice p a b c =
  verifyMeetSemilattice p a b c && identity meet top a

verifyBoundedLattice
  ∷ ∀ a. BoundedLattice a ⇒ Eq a ⇒ Arbitrary a
  ⇒ Proxy a → a → a → a → Boolean
verifyBoundedLattice p a b c
   = verifyBoundedJoinSemilattice p a b c
  && verifyBoundedMeetSemilattice p a b c
  && absorbtion join meet a b

verifyLattice
  ∷ ∀ a. Lattice a ⇒ Eq a ⇒ Arbitrary a
  ⇒ Proxy a → a → a → a → Boolean
verifyLattice p a b c
   = verifyJoinSemilattice p a b c
  && verifyMeetSemilattice p a b c
  && absorbtion join meet a b


main ∷ Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Lattice Boolean" do
    it "should satisfy the laws for Lattice" do
      quickCheck' 1000 $ verifyLattice (Proxy ∷ Proxy Boolean)
  describe "BoundedLattice Boolean" do
    it "should satisfy the laws for BoundedLattice" do
      quickCheck' 1000 $ verifyBoundedLattice (Proxy ∷ Proxy Boolean)
