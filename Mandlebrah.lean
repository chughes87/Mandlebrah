/-- Uniform at random in [0, 1)-/
def randomFloat {gen} [RandomGen gen] (g : gen) : Float × gen :=
  let (n, g') := RandomGen.next g
  let (lo, hi) := RandomGen.range g
  (Float.ofNat (n - lo) / Float.ofNat (hi - lo + 1), g')

def IO.randFloat (lo := 0.0) (hi := 1.0) : IO Float := do
  let gen ← IO.stdGenRef.get
  let (r, gen) := randomFloat gen
  IO.stdGenRef.set gen
  pure $ lo + (hi - lo) * r

structure Complex where
  real: Float
  i: Float
  deriving Repr

def Complex.add (a b: Complex) : Complex :=
  Complex.mk (a.real + b.real) (a.i + b.i)

def Complex.mul (a b: Complex) : Complex :=
  let real : Float := a.real * b.real
  let realComplex : Float := a.real * b.i
  let complexReal : Float := a.i * b.real
  let complex : Float := a.i * b.i
  Complex.mk (real - complex) (realComplex + complexReal)

instance : Add Complex where
  add x y := Complex.add x y

instance : Mul Complex where
  mul := Complex.mul


instance : OfNat Complex x where
  ofNat := Complex.mk (Float.ofNat x) 0


class OfComplex (α : Type _) (c : Complex) where
  ofComplex :α

def Complex.Abs (c : Complex) : Float :=
  Float.sqrt (c.real ^ 2.0 + c.i ^ 2.0)

instance (x: Complex ): OfComplex Float x where
  ofComplex := (Complex.Abs x)

def x : Complex := Complex.mk 0.5 0.5

def Complex.pow : Complex → Nat → Complex
  | c, 0 => 1
  | c, 1 => c
  | c, Nat.succ n => pow (c * c) n

instance : Pow Complex Nat where
  pow := Complex.pow

def step (c z : Complex) : Complex :=
  z ^ 2 + c

def stepIn (bound : Float) (c : Complex) : Nat → Nat → Complex → Nat
  | Nat.zero, currStep, _ => 0
  | Nat.succ maxSteps, currStep, z => match (Complex.Abs z ≥ bound : Bool) with
    | true => currStep
    | false => stepIn bound c maxSteps (Nat.succ currStep) (step c z)

def range (default: a) : Array a → Nat → Array a 
  | acc, 0 => acc
  | acc, Nat.succ x => range default (Array.append acc #[default]) x

def twoDeeRange (default: a) (x y: Nat) : Array (Array a) :=
  range (range default #[] x) #[] y

def buildDimToComplex (xMax yMax : Nat) (cMin cMax : Complex) : Nat → Nat → Complex :=
  let xScalar : Float := (cMax.real - cMin.real) / Float.ofNat xMax
  let yScalar : Float := (cMax.i - cMin.i) / Float.ofNat yMax
  λ (px :Nat) => λ (py: Nat) =>
    cMin + Complex.mk ((Float.ofNat px) * xScalar) ((Float.ofNat py) * yScalar)

def infinity := 2.0
def c: Complex := Complex.mk 0 0

def newScene (px py : Nat) (cMin cMax : Complex) (maxSteps : Nat) : Array (Array Nat) := do
  let mut scene : Array (Array Complex) := twoDeeRange (Complex.mk 0 0) px py
  let dimToComplex := buildDimToComplex px py cMin cMax
  Array.mapIdx scene λ idy dimx =>
    Array.mapIdx dimx λ idx elem=>
      let stepper := stepIn infinity (dimToComplex idx idy) maxSteps 0
      stepper 0

@[inline] def Float.max (x y : Float) : Float := if x ≤ y then y else x
@[inline] def Float.min (x y : Float) : Float := if x ≤ y then x else y

def Float.clampToUInt8 (x : Float) : UInt8 :=
  Float.toUInt8 <| Float.min 255 <| Float.max 0 x

def IO.FS.Handle.writeColor (handle : IO.FS.Handle) (cMax : Nat) (c : Nat) : IO Unit := do
  let r := Float.clampToUInt8 (Float.ofNat c / (Float.ofNat cMax) * 255)
  let g := Float.clampToUInt8 (Float.ofNat c / (Float.ofNat cMax) * 255)
  let b := Float.clampToUInt8 (Float.ofNat c / (Float.ofNat cMax) * 255)
  handle.putStrLn s!"{r} {g} {b}"


def writeFile (filename : String) : IO Unit := do
  IO.println "boop"
  let height := 500
  let width := 500
  let maxSteps := 50
  let pixels := newScene height width (Complex.mk (-1) (-1)) (Complex.mk 1 1) maxSteps

  IO.FS.withFile filename IO.FS.Mode.write λ handle => do
    handle.putStrLn "P3"
    handle.putStrLn s!"{width} {height} 255"
    for i in [0:width] do
      for j in [0:height] do
        let pixel := pixels[i][j]
        handle.writeColor maxSteps pixel

-- def main : List String → IO Unit
-- | [] => writeFile "out.ppm"
-- | (x::xs) => writeFile x


-- #eval writeFile "out.ppm"
def main : IO Unit 
  IO.println "💩"


-- #eval newScene 10 10 (Complex.mk (-1) (-1)) (Complex.mk 1 1)
-- #eval Nat.succ 1
-- #eval x
-- #eval Complex.add (Complex.mk 1 1) (Complex.mk 1 2) 
-- #eval Complex.mul (Complex.mk 1 1) (Complex.mk 1 2) 
-- #eval Complex.mk 1 1 + Complex.mk 11 2
-- #eval x * y
-- #eval x ^ 2
-- #eval Complex.Abs y
-- #eval stepIn 1000000.0 1000000 y 0
