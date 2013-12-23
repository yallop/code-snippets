(* A convenient name for a unary type constructor *) 
module type TyCon = sig type 'a tc end

(* An interface to a weak form of equality that doesn't support Leibniz's law *)
module type WeakEQ =
sig
  (* A value of type (s, t) eq is a proof that types s and t are the same. *)
  type ('a, 'b) eq

  (* The reflexivity axiom. *)
  val refl : ('a, 'a) eq

  (* The symmetry axiom *)
  val symm : ('a, 'b) eq -> ('b, 'a) eq
 
  (* The transitivity axiom *)
  val trans : ('a, 'b) eq -> ('b, 'c) eq -> ('a, 'c) eq

  (* Given a proof that type s and type t are equal, we can convert s to t. *)
  val cast : ('a, 'b) eq -> 'a -> 'b
end

(* An implementation of the weak form of equality.  This is the same as the
   Haskell implementation given in

     Implementing Cut Elimination: A Case Study of Simulating Dependent
     Types in Haskell
     Chiyan Chen, Dengping Zhu and Hongwei Xi
     PADL 2004
*)
module WeakEq : WeakEQ =
struct
  type ('a, 'b) eq = ('a -> 'b) * ('b -> 'a)

  let refl = (fun x -> x), (fun x -> x)
  let symm  (f, g)        = (g, f)
  let trans (f, g) (j, k) = (fun x -> j (f x)), (fun x -> g (k x))
  let cast  (f, g)        = f
end

(* An interface to equality with Leibniz's law as an axiom.  We don't include
   symm and trans, since they are derivable from refl, Subst and cast (see
   below) *)
module type EQ =
sig
  (* A value of type (s, t) eq is a proof that types s and t are the same. *)
  type ('a, 'b) eq

  (* The reflexivity axiom. *)
  val refl : ('a, 'a) eq

  (* Leibniz's substitution axiom. *)
  module Subst (TC : TyCon) :
  sig
    val subst : ('a, 'b) eq -> 'a TC.tc -> 'b TC.tc
  end
end

(* An implementation of Leibniz equality.  This is analogous to the Haskell
   implementation given in

     Typing Dynamic Typing
     Arthur I. Baars and S. Doaitse Swierstra
     ACM SIGPLAN Notices, volume 37 issue 9, 2002
*)
module Eq : EQ =
struct

  (* In Haskell: data EqTC a b = Cast{cast :: forall tc. tc a -> tc b} *)
  module type EqTC =
  sig
    type a and b
    module Cast : functor (TC : TyCon) ->
    sig
      val cast : a TC.tc -> b TC.tc
    end
  end 

  type ('a, 'b) eq = (module EqTC with type a = 'a and type b = 'b)

  let refl (type t) =
    (module struct
       type a = t
       type b = t
       module Cast (TC : TyCon) =
       struct
         let cast v = v
       end
     end : EqTC with type a = t and type b = t)

  module Subst (TC : TyCon) =
  struct
    let subst (type s) (type t) (module S_eq_t : EqTC with type a = s and type b = t) =
      let module M = S_eq_t.Cast(TC) in M.cast
  end
end

include Eq

let cast : 'a 'b. ('a, 'b) eq -> 'a -> 'b =
  fun eq ->
    let module SubstId = Subst(struct type 'a tc = 'a end) in
    SubstId.subst eq

(* We can obtain the symmetry property from subst and refl. *)
let symm : type a b. (a, b) eq -> (b, a) eq =
  fun a_eq_b ->
    let module S = Subst (struct type 'a tc = ('a, a) eq end) in
    (S.subst a_eq_b) refl

(* We can obtain the transitivity property from subst and refl. *)
let trans : type a b c. (a, b) eq -> (b, c) eq -> (a, c) eq =
  fun a_eq_b b_eq_c ->
    let module S = Subst(struct type 'a tc = (a, 'a) eq end) in
    (S.subst b_eq_c) a_eq_b

(* If a = b then f a = f b *)
module Lift (TC : TyCon) :
sig
  val lift : ('a, 'b) eq -> ('a TC.tc, 'b TC.tc) eq
end =
struct
  let lift (type s) (type t) (eq : (s, t) eq)  =
    let module S = Subst(struct type 'a tc = (s TC.tc, 'a TC.tc) eq end) in
    S.subst eq refl
end

(* Our implementation of equality seems sufficient for the common examples,
   but has one apparent limitation, described below.

   A few examples seem to require an inverse of Leibniz's law.  For injectivty
   type constructors t, we would like to have

   ('a t, 'b t) eq -> ('a, 'b) eq

   For example, given a proof that two function types are equal, we would like
   to extract proofs that the domain and codomain types are equal:

   ('a -> 'b, 'c -> 'd) eq -> ('a, 'c) eq * ('b, 'd) eq

   GADTs themselves support type decomposition in this way.
*)

(* Unfortunately, injectivity is supported only for WeakEq.eq.
   We may always get WeakEq.eq from EQ.eq.
*)
let degrade : type r s. (r, s) eq -> (r, s) WeakEq.eq =
  fun r_eq_s ->
    let module M = Eq.Subst(struct type 'a tc = ('a, r) WeakEq.eq end) in
    WeakEq.symm (M.subst r_eq_s (WeakEq.refl))
