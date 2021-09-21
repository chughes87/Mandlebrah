universe u v w

/- 
A type class to represent closed intervals
-/
---------------- The Interval   The value
class Interval (ω : Type w) (α : Type v) where
  contains : ω → α → Bool
  overlaps : ω → α → Bool
  subInterval :  ω → ω → Bool
  split : ω → α → ω × ω

structure NatInterval where
  l : Nat
  u : Nat

namespace NatInterval


variable (n : NatInterval)

def bld (l₁ u₁ : Nat) : NatInterval := 
  match (l₁ < u₁ : Bool) with
  | true => {l := l₁, u:= u₁}
  | false => {l := u₁, u:= l₁}

def isEmpty : Bool := n.l == n.u

def contains (hn : Nat) : Bool := n.l ≤ hn ∧ hn ≤ n.u

def overlaps (hni : NatInterval) : Bool := 
  -- If either the lower bound or the upper
  -- bound are contained then the interval overlaps
  n.contains hni.l ∨ n.contains hni.u

def isSubInterval (hni : NatInterval) : Bool :=
  n.contains hni.l ∧ n.contains hni.u

#check bld 1 1
def split (splitAt : Nat ) : NatInterval × NatInterval := 
  (bld n.l splitAt, bld splitAt n.u)


end NatInterval

#check Interval Nat Nat

scoped 
--instance : Interval Nat Nat where
--  isBounded := 
--  contains: ω → Bool
--  cut: ω → Interval × Interval


inductive QNode (α : Type u) (bound: (w × w) × (w × w)) where
  | leaf : QNode α bound
  | node  (n₀₀ : QNode α bound) 
          (n₀₁ : QNode α bound) 
          (n₁₀ : QNode α bound) 
          (n₁₁ : QNode α bound) : QNode α bound

 namespace QNode 
 variable {α : Type u} {δ : Nat}

 open Nat

 def depth 
