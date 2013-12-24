(* Positive and negative contexts *)
module type POS = sig type +'a t end
module type NEG = sig type -'a t end

(* a is a subtype of b if a can be coerced to b in a positive context *)
module type SubPos =
sig
  type a and b
  module Coerce (Pos : POS) : sig val cast : a Pos.t -> b Pos.t end
  end
type ('a, 'b) sub = (module SubPos with type a = 'a and type b = 'b)

(* Alternatively, a is a subtype of b if b can be coerced to a in a negative
   context *)
module type SubNeg =
sig
  type a and b
  module Coerce (Neg : NEG) : sig val cast : b Neg.t -> a Neg.t end
end
type ('a, 'b) subneg = (module SubNeg with type a = 'a and type b = 'b)

(* We can freely convert between the two definitions of subtyping *)
val sub_of_subneg : ('a, 'b) subneg -> ('a, 'b) sub
val subneg_of_sub : ('a, 'b) sub -> ('a, 'b) subneg

(* We can obtain an upcast in a positive context from a proof of subtyping *)
module SubstPos (Pos : POS) :
sig val cast : ('a, 'b) sub -> 'a Pos.t -> 'b Pos.t end

(* We can obtain an upcast in a negative context from a proof of subtyping *)
module SubstNeg (Neg : NEG) :
sig val cast : ('a, 'b) sub -> 'b Neg.t -> 'a Neg.t end

(* (The identity context is positive) *)
val cast : ('a, 'b) sub -> 'a -> 'b

(* Subtyping is reflexive *)
val refl : ('a, 'a) sub

(* Subtyping is transitive *)
val trans : ('a, 'b) sub -> ('b, 'c) sub -> ('a, 'c) sub

(* Positive contexts preserve subtyping *)
module LiftPlus (Pos : POS) :
sig val lift : ('a, 'b) sub -> ('a Pos.t, 'b Pos.t) sub end

(* Negative contexts flip the subtyping relation *)
module LiftMinus (Neg : NEG) :
sig val lift : ('a, 'b) sub -> ('b Neg.t, 'a Neg.t) sub end

(* The supertyping relation is the flipped subtyping relation *)
type ('a, 'b) sup = ('b, 'a) sub
