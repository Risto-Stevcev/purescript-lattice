module Data.Lattice.Verify where

import Prelude (class Eq, (&&))
import Type.Proxy (Proxy)
import Data.Lattice
import Control.Algebra.Properties (absorbtion, associative, commutative,
                                   idempotent', identity)

verifySemilattice ∷ ∀ a. Eq a ⇒ (a → a → a) → a → a → a → Boolean
verifySemilattice f a b c =
  associative f a b c && commutative f a b && idempotent' f a

verifyJoinSemilattice ∷ ∀ a. JoinSemilattice a ⇒ Eq a ⇒ a → a → a → Boolean
verifyJoinSemilattice = verifySemilattice join

verifyMeetSemilattice
  ∷ ∀ a. MeetSemilattice a ⇒ Eq a ⇒ a → a → a → Boolean
verifyMeetSemilattice = verifySemilattice meet

verifyBoundedJoinSemilattice
  ∷ ∀ a. BoundedJoinSemilattice a ⇒ Eq a ⇒ a → a → a → Boolean
verifyBoundedJoinSemilattice a b c =
  verifyJoinSemilattice a b c && identity join bottom a

verifyBoundedMeetSemilattice
  ∷ ∀ a. BoundedMeetSemilattice a ⇒ Eq a ⇒ a → a → a → Boolean
verifyBoundedMeetSemilattice a b c =
  verifyMeetSemilattice a b c && identity meet top a

verifyBoundedLattice ∷ ∀ a. BoundedLattice a ⇒ Eq a ⇒ a → a → a → Boolean
verifyBoundedLattice a b c
   = verifyBoundedJoinSemilattice a b c
  && verifyBoundedMeetSemilattice a b c
  && absorbtion join meet a b

verifyLattice ∷ ∀ a. Lattice a ⇒ Eq a ⇒ a → a → a → Boolean
verifyLattice a b c
   = verifyJoinSemilattice a b c
  && verifyMeetSemilattice a b c
  && absorbtion join meet a b

verify ∷ ∀ a. Eq a ⇒ Proxy a → (a → a → a → Boolean) → a → a → a → Boolean
verify _ p = p
