module Data.Lattice where

import Prelude

-- | A `JoinSemilattice` must satisfy the following laws:
-- |
-- | + Associativity of join:
-- |     forall a b c, join a (join b c) == join (join a b) c
-- | + Commutativity of join:
-- |     forall a b,   join a b          == join b a
-- | + Idempotency of join:
-- |     forall a,     join a a          == a
-- |
-- | Join semilattices capture the notion of sets with a "least upper bound".
class JoinSemilattice a where
  join ∷ a -> a -> a


-- | A `MeetSemilattice` must satisfy the following laws:
-- |
-- | + Associativity of meet:
-- |     forall a b c, meet a (meet b c) == meet (meet a b) c
-- | + Commutativity of meet:
-- |     forall a b,   meet a b          == meet b a
-- | + Idempotency of meet:
-- |     forall a,     meet a a          == a
-- |
-- | Meet semilattices capture the notion of sets with a "greatest lower bound".
class MeetSemilattice a where
  meet ∷ a -> a -> a


-- | A `BoundedJoinSemilattice` must satisfy the following laws in addition to
-- | `JoinSemilattice` laws:
-- |
-- | + Bottom (Unitary Element):
-- |     forall a,     join a bottom     == a
-- |
-- |  Join semilattices capture the notion of sets with a "least upper bound"
-- |  equipped with a "bottom" element.
class JoinSemilattice a ⇐ BoundedJoinSemilattice a where
  bottom ∷ a


-- | A `BoundedMeetSemilattice` must satisfy the following laws in addition to
-- | `MeetSemilattice` laws:
-- |
-- | +  Top (Unitary Element):
-- |     forall a,     meet a top        == a
-- |
-- | Meet semilattices capture the notion of sets with a "greatest lower bound"
-- | equipped with a "top" element.
class MeetSemilattice a ⇐ BoundedMeetSemilattice a where
  top ∷ a


-- | A `Lattice` must satisfy the following in addition to `JoinSemilattice`
-- | and `MeetSemilattice` laws:
-- |
-- | + Absorbtion laws for meet and join:
-- |     forall a b,   meet a (join a b) == a
-- |     forall a b,   join a (meet a b) == a
class (JoinSemilattice a, MeetSemilattice a) ⇐ Lattice a


-- | A `BoundedLattice` must satisfy the following in addition to
-- | `BoundedMeetSemilattice` and `BoundedJoinSemilattice` laws:
-- |
-- | + Absorbtion laws for meet and join:
-- |     forall a b,   meet a (join a b) == a
-- |     forall a b,   join a (meet a b) == a
class (BoundedJoinSemilattice a, BoundedMeetSemilattice a) ⇐ BoundedLattice a


instance joinSemilatticeBoolean ∷ JoinSemilattice Boolean where
  join = (||)

instance meetSemilatticeBoolean ∷ MeetSemilattice Boolean where
  meet = (&&)

instance boundedJoinSemilatticeBoolean ∷ BoundedJoinSemilattice Boolean where
  bottom = false

instance boundedMeetSemilatticeBoolean ∷ BoundedMeetSemilattice Boolean where
  top = true

instance latticeBoolean ∷ Lattice Boolean

instance boundedLatticeBoolean ∷ BoundedLattice Boolean
