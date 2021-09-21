
inductive spec_float 
  | S754_zero (s : Bool)
  | S754_infinity (s : Bool)
  | S754_nan
  | S754_finite (s : Bool) (m: Nat) (e : Int)

#eval max 1 22
namespace FloatOps
  variable (prec emax : Int)

  def emin : Int := (3 - emax - prec) 
  def fexp (e : Int) : Int := max (e - prec) (3 - emax - prec)
  

  section Zdigits
  end Zdigits



end FloatOps