structure Complex where
  real: Float
  i: Float
  deriving Repr

namespace Complex 
def add (a b: Complex) : Complex :=
  Complex.mk (a.real + b.real) (a.i + b.i)

def mul (a b: Complex) : Complex :=
  let real : Float := a.real * b.real
  let realComplex : Float := a.real * b.i
  let complexReal : Float := a.i * b.real
  let complex : Float := a.i * b.i
  Complex.mk (real - complex) (realComplex + complexReal)

instance : Add Complex where
  add x y := Complex.add x y

instance : Neg Complex where
  neg x := Complex.mk (-x.real) (-x.i) 

instance : Sub Complex where
  sub x y := Complex.add x (-y)

instance : Mul Complex where
  mul := Complex.mul

instance : HMul Complex Float Complex where
  hMul x y := Complex.mk (x.real * y)  (x.i * y) 

instance : OfNat Complex x where
  ofNat := Complex.mk (Float.ofNat x) 0

class OfComplex (α : Type _) (c : Complex) where
  ofComplex :α

def Abs (c : Complex) : Float :=
  Float.sqrt (c.real ^ 2.0 + c.i ^ 2.0)

instance : LT Complex where
  lt x y := (Abs (x - y)) < 0

instance (x: Complex ): OfComplex Float x where
  ofComplex := (Complex.Abs x)

def pow : Complex → Nat → Complex
  | c, 0 => 1
  | c, 1 => c
  | c, Nat.succ n => pow (c * c) n

instance : Pow Complex Nat where
  pow := Complex.pow

end Complex