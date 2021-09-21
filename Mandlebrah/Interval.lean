universe u v w

/- 
  contains : ω → α → Bool
  overlaps : ω → α → Bool
  subInterval :  ω → ω → Bool
  split : ω → α → ω × ω
A type class to represent closed intervals
-/
---------------- The Interval   The value
class Interval (α : Type v) (β : Type w)  where
  max : α -> β 
  min : α -> β  


def NatInterval := Nat × Nat
namespace NatInterval


variable (n : NatInterval)
@[inline] def ordered: Nat × Nat := if n.fst > n.snd then n else (n.snd, n.fst) 
@[inline] def max: Nat := n.ordered.fst
@[inline] def min: Nat := n.ordered.snd 
@[inline] def l := n.min
@[inline] def u := n.max

def isEmpty : Bool := n.l == n.u

def contains (hn : Nat) : Bool := n.l ≤ hn ∧ hn ≤ n.u

def overlaps (hni : NatInterval) : Bool := 
  -- If either the lower bound or the upper
  -- bound are contained then the interval overlaps
  n.contains hni.l ∨ n.contains hni.u

def isSubInterval (hni : NatInterval) : Bool :=
  n.contains hni.l ∧ n.contains hni.u

def split (splitAt : Nat ) : NatInterval × NatInterval := 
  ((n.l, splitAt), (splitAt, n.u))


end NatInterval

instance Interval NatInterval Nat
  upper := 


#check Interval Nat Nat

