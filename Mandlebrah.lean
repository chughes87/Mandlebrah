import Mandlebrah.Complex

def step (c z : Complex) : Complex := z ^ 2 + c

def mandel (c : Complex) (bound : Float := 2.0) (maxSteps : Nat := 20) : Nat :=
  let rec iterate : Complex → Nat → Nat → Nat := λ  
  | _, 0, _ => 0
  | z, maxSteps + 1, currStep => 
      match (Complex.Abs z ≥ bound : Bool) with
        | true => currStep
        | false => iterate (step c z) maxSteps currStep.succ 
  iterate 0 maxSteps 0

def twoDeeRange (default: a) (x y: Nat) : Array (Array a) :=
  Array.mkArray y <| Array.mkArray x default

def buildDimToComplex (xMax yMax : Nat) (cMin cMax : Complex) : Nat → Nat → Complex :=
  let xScalar : Float := (cMax.real - cMin.real) / Float.ofNat xMax
  let yScalar : Float := (cMax.i - cMin.i) / Float.ofNat yMax
  fun (px py : Nat) => cMin + Complex.mk ((Float.ofNat px) * xScalar) ((Float.ofNat py) * yScalar)

section x
  variable (size : Nat) 
  variable (hpx hpy: Nat) 
  variable (hcMin hcMax : Complex) 
  variable (hp: hpx < size ∧  hpy < size ∧ hpx >0 ∧ hpy > 0 )
  variable (hc: hcMin < hcMax) 
  variable (hsize: size > 0)
  def ap: Nat → Nat → Complex := 
    (buildDimToComplex  size size hcMin hcMax) 

#check ap

  theorem BDimIdAtZero : hcMin = (buildDimToComplex size size hcMin hcMax 0 0 ) := by
    apply Eq.symm
    sorry

  theorem existsAIncrement : ∃ x : Float, ∀ p : Nat, ∀ q: Nat,  
                                            (buildDimToComplex size size hcMin hcMax p q) -
                                            (buildDimToComplex size size hcMin hcMax (p+1) (q+1)) * x = 0 :=
                                          
                                            sorry
 

  theorem DimInBounds : hcMin < (buildDimToComplex size size hcMin hcMax hpy hpx) ∧ 
             hcMax > (buildDimToComplex size size hcMin hcMax hpy hpx) := by sorry
 
 #check size < hpx
--  theorem dimToComplexBounded : hcomp := by


  sorry

end x

def newScene (px py : Nat) (cMin cMax : Complex) (maxSteps : Nat) : Array (Array Nat) := do
  let mut scene : Array (Array Complex) := twoDeeRange (Complex.mk 0 0) px py
  let dimToComplex := buildDimToComplex px py cMin cMax
  Array.mapIdx scene λ idy dimx =>
    Array.mapIdx dimx λ idx elem =>
      (dimToComplex idx idy) |> mandel
      
@[inline] def Float.max (x y : Float) : Float := if x ≤ y then y else x
@[inline] def Float.min (x y : Float) : Float := if x ≤ y then x else y

open Float in
  def Float.clampToUInt8 (x : Float) : UInt8 :=
    toUInt8 <| min 255 <| max 0 x

def IO.FS.Handle.writeColor (handle : IO.FS.Handle) (cMax : Nat) (c : Nat) : IO Unit := do
  let r := Float.clampToUInt8 (Float.ofNat c / (Float.ofNat cMax) * 255)
  let g := Float.clampToUInt8 (Float.ofNat c / (Float.ofNat cMax) * 255)
  let b := Float.clampToUInt8 (Float.ofNat c / (Float.ofNat cMax) * 255)
  handle.putStrLn s!"{r} {g} {b}"

def writeFile (filename : String) (maxSteps := 50) (width := 500)  (height := 500)  : IO Unit := do
  IO.println "boop"
  let pixels := newScene height width (Complex.mk (-1) (-1)) (Complex.mk 1 1) maxSteps

  IO.FS.withFile filename IO.FS.Mode.write λ handle => do
    handle.putStrLn "P3"
    handle.putStrLn s!"{width} {height} 255"
    for i in [0:width] do
      for j in [0:height] do
        let pixel := pixels[i][j]
        handle.writeColor maxSteps pixel

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
-- #eval writeFile "out.ppm"
def main : List String → IO Unit
  | [] => writeFile "out.ppm"
  | (x::xs) => writeFile x